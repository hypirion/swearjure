{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Primitives where

import Control.Monad.Except
import Data.Generics.Fixplate
import Swearjure.AST hiding (lookup)
import Swearjure.Errors

getFn :: [Expr] -> EvalState Expr
getFn [m, v] = getFn [m, v, Fix Nil]
getFn [m, k, default'] = find (unFix m) k default'
  where find (ESet vals) v d
          | v `elem` vals = return v
          | otherwise = return d
        find (EVec vals) (Fix (EInt n)) d
          | n < 0 = return d
          | length vals > fromIntegral n = return $ vals !! fromIntegral n
          | otherwise = return d
        find (EVec _) _ d = return d
        find (EHM pairs) v d = case lookup v pairs of
                                Just res -> return res
                                Nothing -> return d
        find _ _ d = return d
getFn x = throwError $ ArityException (length x) "core/get"

get1Fn :: [Expr] -> EvalState Expr
get1Fn = undefined

-- this one must be wrapped properly to use in envs.
seq :: [Expr] -> EvalState [Expr]
seq [x] = go (unFix x)
  where go (ESet vals) = return vals
        go (EList vals) = return vals
        go (EVec vals) = return vals
        go (EHM pairs) = return $ vecPairs pairs
        go _ = throwError $ CastException "some thing" "ISeq"
        vecPairs = map (\(a, b) -> (Fix (EVec [a, b])))
seq x = throwError $ ArityException (length x) "core/seq"
