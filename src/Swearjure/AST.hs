{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Swearjure.AST where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Generics.Fixplate
import qualified Data.Map as M
import           Swearjure.Errors
import           Text.PrettyPrint

type NS = String

data Env' e = Toplevel (M.Map String (NS, e))
            | Nested (Env' e) (M.Map String (NS, e))
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Env = Env' Expr

-- this would've been namespaced if this was proper clojure, but you can't refer
-- to same-named values in different namespaces in Swearjure, so it doesn't
-- matter.
lookup :: (MonadReader Env m, MonadError SwjError m) =>
          String -> m Expr
lookup s = ask >>= lookupRec
  where lookupRec (Toplevel m)
          = m `findOr` (throwError $ NotFound s)
        lookupRec (Nested up m)
          = m `findOr` lookupRec up
        m `findOr` other = do let v = M.lookup s m
                              maybe other (return . snd) v

getMapping :: (MonadReader Env m) => String -> m (Maybe Expr)
getMapping s = ask >>= lookupRec
  where lookupRec (Toplevel m) = m `findOr` return Nothing
        lookupRec (Nested up m) = m `findOr` lookupRec up
        m `findOr` other = do let v = M.lookup s m
                              maybe other (namespaced . fst) v
        namespaced ns = return . Just . Fix . ESym (Just ns) $ s


data Fn' e = Fn (Env' e) String String [([String], Maybe String, e)]
           deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Fn = Fn' Expr

-- Again I fail at naming.
data SwjExp e = ESym (Maybe String) String
              | EStr String
              | EInt Integer
              | ERatio Integer Integer
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
        go (ERatio num den) = integer num <> char '/' <> integer den
        go (EFloat f) = double f
        go (EChar c) = char '\\' <> char c -- prettyprint-dependent
        go (EList xs) = parens $ hsep xs
        go (EVec xs) = brackets $ hsep xs
        go (EHM pairs) = braces $ hsep $ concatMap (\(x,y) -> [x, y]) pairs
        go (ESet s) = char '#' <> (braces $ hsep s)
        go (EKw ns s) = char ':' <> nsPP ns <> text s
        go (EBool True) = text "true"
        go (EBool False) = text "false"
        go (EFn (Fn _ ns fname _)) = text "#<" <> text ns <> char '$'
                                     <> text fname <> char '>'
        go Nil = text "nil"
        nsPP (Just ns) = text ns <> char '/'
        nsPP Nothing = empty
