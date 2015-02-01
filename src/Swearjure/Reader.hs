{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Reader where

import Control.Monad
import Data.Generics.Fixplate
import Swearjure.AST
import Swearjure.Errors
import Swearjure.Parser

readExpr :: String -> Either SwjError (Maybe Expr)
readExpr = readAst >=> convertAst

convertAst :: Maybe PVal -> Either SwjError (Maybe Expr)
convertAst Nothing = return Nothing
convertAst (Just ast) = (return . Just . cata (Fix . go)) ast
  where go (PSym s) = ESym Nothing s
        go (PString s) = EStr s
        go (PKw s) = EKw Nothing s
        go (PQualKw s) = EKw (Just "user") s
        -- -^ if we could move nses, this would've forced this fn to be of
        -- EvalState
        go (PChar c) = EChar c
        go (PList xs) = EList xs
        go (PVec xs) = EVec xs
        go (PSet xs) = ESet xs
        -- want this to be Data.Set, but forces eq + ord on Mu/Attr
        go (PHM pairs) = EHM pairs -- Same here, ugh.
