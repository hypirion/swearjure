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


type EvalState = ReaderT Env (StateT Int (ExceptT SwjError IO))

type NS = String

data EnvF e = Toplevel (M.Map String (NS, (Bool, e)))
            | Nested (EnvF e) (M.Map String (NS, (Bool, e)))
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Env = EnvF Val

-- lookup and lookupMacro would've been namespaced if this was proper clojure,
-- but you can't refer to same-named values in different namespaces in
-- Swearjure, so it doesn't matter.
lookupMacro :: (MonadReader Env m) => String -> m (Maybe Val)
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
          String -> m Val
lookup s = ask >>= lookupRec
  where lookupRec (Toplevel m) = m `findOr` throwError (NotFound s)
        lookupRec (Nested up m) = m `findOr` lookupRec up
        m `findOr` other
          = do let v = M.lookup s m
               case v of
                Just (_, (False, val)) -> return val
                Just (ns, (True, _)) -> throwError $ IllegalArgument $
                                        "Can't take value of macro: #'" ++ ns
                                        ++ "/" ++ s
                Nothing -> other

extendEnv :: (MonadReader Env m) => Env -> [(String, Val)] -> m a -> m a
extendEnv oldEnv mappingList = local extend
  where extend _ = Nested oldEnv mapping
        mapping = M.fromList $ map (\(x, y) -> (x, ("user", (False, y))))
                  mappingList

specials :: S.Set String
specials = S.fromList
           ["fn*", "quote", ".", "var", "&", "if", "nil"]

getMapping :: (MonadReader Env m) => String -> m (Maybe Val)
getMapping s = if S.member s specials
               then return . Just . Fix . ESym Nothing $ s
               else ask >>= lookupRec
  where lookupRec (Toplevel m) = m `findOr` return Nothing
        lookupRec (Nested up m) = m `findOr` lookupRec up
        m `findOr` other = do let v = M.lookup s m
                              maybe other (namespaced . fst) v
        namespaced ns = return . Just . Fix . ESym (Just ns) $ s

data FnF e = Fn { fnEnv :: EnvF e
                , fnNs :: String
                , fnName :: String
                , fnRecName :: Maybe String
                , fnFns :: [([String], Maybe String, [e])]
                }
           | PrimFn (PFn Val)
           deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PFn e = Prim (String, String) ([e] -> EvalState e)

instance Eq (PFn e) where
  (Prim a _) == (Prim b _) = a == b

instance Ord (PFn e) where
  (Prim a _) `compare` (Prim b _) = a `compare` b

instance Show (PFn e) where
  show (Prim (ns, name) _) = "#<" ++ show ns ++ "$" ++ show name ++ ">"

type Fn = FnF Val

data SwjValF e = ESym (Maybe String) String
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
               | EFn (FnF e)
               | Nil
               deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

typeName' :: SwjValF e -> String
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

typeName :: Val -> String
typeName = typeName' . unFix

type Val = Mu SwjValF

iList :: [Val] -> Val
iList = Fix . EList

iVec :: [Val] -> Val
iVec = Fix . EVec

iHM :: [(Val, Val)] -> Val
iHM = Fix . EHM

iSet :: [Val] -> Val
iSet = Fix . ESet

instance EqF SwjValF where equalF = (==)
instance OrdF SwjValF where compareF = compare
instance ShowF SwjValF where showsPrecF = showsPrec

prStr :: Val -> String
prStr = show . pp' sfn
  where sfn str = char '"' <> text (unquote str) <> char '"'
        unquote = concatMap uq
        uq '\\' = "\\\\"
        uq '"' = "\\\""
        uq x = [x]

pp' :: (String -> Doc) -> Val -> Doc
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
        go (ESet s) = char '#' <> braces (hsep s)
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

_quote :: Val
_quote = Fix $ ESym Nothing "quote"

_unquote :: Val
_unquote = Fix $ ESym (Just "clojure.core") "unquote"

_unquoteSplicing :: Val
_unquoteSplicing = Fix $ ESym (Just "clojure.core") "unquote-splicing"

_seq :: Val
_seq = Fix $ ESym (Just "clojure.core") "seq"

_concat :: Val
_concat = Fix $ ESym (Just "clojure.core") "concat"

_apply :: Val
_apply = Fix $ ESym (Just "clojure.core") "apply"

_list :: Val
_list = Fix $ ESym (Just "clojure.core") "list"

_vector :: Val
_vector = Fix $ ESym (Just "clojure.core") "vector"

_hashset :: Val
_hashset = Fix $ ESym (Just "clojure.core") "hash-set"

_hashmap :: Val
_hashmap = Fix $ ESym (Just "clojure.core") "hash-map"

_nil :: Val
_nil = Fix Nil

_fnStar :: Val
_fnStar = Fix $ ESym Nothing "fn*"
