{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Swearjure.AST where

import Data.Generics.Fixplate
import Text.PrettyPrint

data SwjExp e = ESym (Maybe String) String
              | EStr String
              | EInt Integer
              | ERatio Integer Integer
              | EFloat Double
              | EChar Char
              | EList [e]
              | EFnLit [e]
              | EVec [e]
              | EHM [(e, e)]
              | ESet [e]
              | EKw (Maybe String) String
              | EBool Bool
              | Nil
                -- howto funs? gahhh. Bane of my existence.
              deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr = Mu SwjExp

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
        go (EFnLit xs) = char '#' <> (parens $ hsep xs)
        go (EList xs) = parens $ hsep xs
        go (EVec xs) = brackets $ hsep xs
        go (EHM pairs) = braces $ hsep $ concatMap (\(x,y) -> [x, y]) pairs
        go (ESet s) = char '#' <> (braces $ hsep s)
        go (EKw ns s) = char ':' <> nsPP ns <> text s
        go (EBool True) = text "true"
        go (EBool False) = text "false"
        go Nil = text "nil"
        nsPP (Just ns) = text ns <> char '/'
        nsPP Nothing = empty
