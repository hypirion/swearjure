{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Swearjure.AST where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Generics.Fixplate
import qualified Data.Map as M
import           Data.Ratio
import qualified Data.Set as S
import           Swearjure.Errors
import           Text.PrettyPrint


type EvalState = ReaderT Env (StateT Int (Except SwjError))

type NS = String

data Env' e = Toplevel (M.Map String (NS, (Bool, e)))
            | Nested (Env' e) (M.Map String (NS, (Bool, e)))
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Env = Env' Expr

-- lookup and lookupMacro would've been namespaced if this was proper clojure,
-- but you can't refer to same-named values in different namespaces in
-- Swearjure, so it doesn't matter.
lookupMacro :: (MonadReader Env m) => String -> m (Maybe Expr)
lookupMacro s = ask >>= lookupRec
  where lookupRec (Toplevel m) = m `findOr` return Nothing
        lookupRec (Nested up m) = m `findOr` lookupRec up
        m `findOr` other
          = do let v = M.lookup s m
               case v of
                Just (_, (False, _)) -> return Nothing
                Just (_, (True, macro)) -> return (Just macro)
                Nothing -> other

lookup :: (MonadReader Env m, MonadError SwjError m) =>
          String -> m Expr
lookup s = ask >>= lookupRec
  where lookupRec (Toplevel m) = m `findOr` (throwError $ NotFound s)
        lookupRec (Nested up m) = m `findOr` lookupRec up
        m `findOr` other
          = do let v = M.lookup s m
               case v of
                Just (_, (False, val)) -> return val
                Just (ns, (True, _)) -> throwError $ IllegalArgument $
                                        "Can't take value of a macro: #'" ++ ns
                                        ++ "/" ++ s
                Nothing -> other

extendEnv :: (MonadReader Env m) => Env -> [(String, Expr)] -> m a -> m a
extendEnv oldEnv mappingList = local extend
  where extend _ = Nested oldEnv mapping
        mapping = M.fromList $ map (\(x, y) -> (x, ("user", (False, y))))
                  mappingList

specials :: S.Set String
specials = S.fromList
           ["fn*", "quote", ".", "var", "&", "if", "nil"]

getMapping :: (MonadReader Env m) => String -> m (Maybe Expr)
getMapping s = if S.member s specials
               then return . Just . Fix . ESym Nothing $ s
               else ask >>= lookupRec
  where lookupRec (Toplevel m) = m `findOr` return Nothing
        lookupRec (Nested up m) = m `findOr` lookupRec up
        m `findOr` other = do let v = M.lookup s m
                              maybe other (namespaced . fst) v
        namespaced ns = return . Just . Fix . ESym (Just ns) $ s

data Fn' e = Fn { fnEnv :: (Env' e)
                , fnNs :: String
                , fnName :: String
                , fnRecName :: Maybe String
                , fnFns :: [([String], Maybe String, [e])]
                }
           | PrimFn (PFn Expr)
           deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PFn e = Prim (String, String) ([e] -> EvalState e)

instance Eq (PFn e) where
  (Prim a _) == (Prim b _) = a == b

instance Ord (PFn e) where
  (Prim a _) `compare` (Prim b _) = a `compare` b

instance Show (PFn e) where
  show (Prim (ns, name) _) = "#<" ++ show ns ++ "$" ++ show name ++ ">"

type Fn = Fn' Expr

-- Again I fail at naming.
data SwjExp e = ESym (Maybe String) String
              | EStr String
              | EInt Integer
              | ERatio Rational
              | EFloat Double
              | EChar Char
              | EList [e]
              | EVec [e]
              | EHM [(e, e)]
              | ESet [e]
              | EKw (Maybe String) String
              | EBool Bool
              | EFn (Fn' e)
              | Nil
              deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

typeName' :: SwjExp e -> String
typeName' (ESym _ _) = "Symbol"
typeName' (EStr _ ) = "String"
typeName' (EInt _) = "Integer"
typeName' (ERatio _) = "Ratio"
typeName' (EFloat _) = "Float"
typeName' (EChar _) = "Char"
typeName' (EList _) = "PersistentList"
typeName' (EVec _) = "PersistentVector"
typeName' (EHM _) = "PersistentHashMap"
typeName' (ESet _) = "PersistentSet"
typeName' (EKw _ _) = "Keyword"
typeName' (EBool _) = "Boolean"
typeName' (EFn _) = "Fn"
typeName' Nil = "nil"

typeName :: Expr -> String
typeName = typeName' . unFix

type Expr = Mu SwjExp

iList :: [Expr] -> Expr
iList = Fix . EList

iVec :: [Expr] -> Expr
iVec = Fix . EVec

iHM :: [(Expr, Expr)] -> Expr
iHM = Fix . EHM

iSet :: [Expr] -> Expr
iSet = Fix . ESet

instance EqF SwjExp where equalF = (==)
instance OrdF SwjExp where compareF = compare
instance ShowF SwjExp where showsPrecF = showsPrec

prStr :: Expr -> String
prStr = show . pp' sfn
  where sfn str = char '"' <> text (unquote str) <> char '"'
        unquote = concatMap uq
        uq '\\' = "\\\\"
        uq '"' = "\\\""
        uq x = [x]

pp' :: (String -> Doc) -> Expr -> Doc
pp' sfn = cata go
  where go (ESym ns s) = nsPP ns <> text s
        go (EStr s) = sfn s
        go (EInt n) = integer n
        go (ERatio r) = integer (numerator r) <> char '/' <> integer (denominator r)
        go (EFloat f) = double f
        go (EChar c) = char '\\' <> char c -- prettyprint-dependent
        go (EList xs) = parens $ hsep xs
        go (EVec xs) = brackets $ hsep xs
        go (EHM pairs) = braces $ hsep $ concatMap (\(x,y) -> [x, y]) pairs
        go (ESet s) = char '#' <> (braces $ hsep s)
        go (EKw ns s) = char ':' <> nsPP ns <> text s
        go (EBool True) = text "true"
        go (EBool False) = text "false"
        go (EFn Fn { fnNs = ns, fnName = fname})
          = text "#<" <> text ns <> char '$' <> text fname <> char '>'
        go (EFn (PrimFn (Prim (ns, fname) _)))
          = text "#<" <> text ns <> char '$' <> text fname <> char '>'
        go Nil = text "nil"
        nsPP (Just ns) = text ns <> char '/'
        nsPP Nothing = empty

_quote :: Expr
_quote = Fix $ ESym Nothing "quote"

_unquote :: Expr
_unquote = Fix $ ESym (Just "clojure.core") "unquote"

_unquoteSplicing :: Expr
_unquoteSplicing = Fix $ ESym (Just "clojure.core") "unquote-splicing"

_seq :: Expr
_seq = Fix $ ESym (Just "clojure.core") "seq"

_concat :: Expr
_concat = Fix $ ESym (Just "clojure.core") "concat"

_apply :: Expr
_apply = Fix $ ESym (Just "clojure.core") "apply"

_list :: Expr
_list = Fix $ ESym (Just "clojure.core") "list"

_vector :: Expr
_vector = Fix $ ESym (Just "clojure.core") "vector"

_hashset :: Expr
_hashset = Fix $ ESym (Just "clojure.core") "hash-set"

_hashmap :: Expr
_hashmap = Fix $ ESym (Just "clojure.core") "hash-map"

_nil :: Expr
_nil = Fix Nil

_fnStar :: Expr
_fnStar = Fix $ ESym Nothing "fn*"
