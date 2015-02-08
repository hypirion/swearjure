{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Eval where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Except
import           Data.Generics.Fixplate (Mu(..))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Traversable as T
import           Prelude hiding (lookup, seq, concat)
import           Swearjure.AST
import           Swearjure.Errors
import           Swearjure.Primitives


initEnv :: Env
initEnv = Toplevel $ M.fromList $ map
          (\(fname, f) -> (fname, ("clojure.core",
                                   Fix $ EFn $ PrimFn (Prim ("core", fname) f))))
          [ ("+", plus)
          , ("/", divFn)
          , ("*", mul)
          , ("-", minus)
          , ("list", return . Fix . EList)
          , ("vector", return . Fix . EVec)
          , ("apply", apply)
          , ("seq", liftM (Fix . EList) . seq)
          , ("concat", liftM (Fix . EList) . concat)
          , ("<", lt)
          , (">", gt)
          , ("<=", lte)
          , (">=", gte)
          , ("=", eq)
          , ("==", numEq)
          ]

apply :: [Expr] -> EvalState Expr
apply [] = throwError $ ArityException 0 "core/apply"
apply [_] = throwError $ ArityException 1 "core/apply"
apply (f : xs) = do fn <- ifn f
                    lastOnes <- seq [last xs]
                    let spliced = init xs ++ lastOnes
                    case fn of
                     PrimFn (Prim _ prim) -> prim spliced
                     (Fn _ _ _ _) -> throwError $ IllegalState "Can't apply nonprimitives yet"

eval :: Expr -> EvalState Expr
eval = go . unFix
  where go (ESym _ s) = lookup s
        go x@(EList []) = return $ Fix x
        go (EList xs)
          | head xs == _quote = return $ fromMaybe _nil (listToMaybe $ tail xs)
          | head xs == _nil = throwError $ IllegalArgument "Can't call nil"
          | otherwise = do f : xs' <- mapM eval xs
                           apply $ [f, (Fix (EList xs'))]
        go v@(EVec _) = Fix <$> T.mapM eval v
        go (ESet xs) = do evals <- mapM eval xs
                          -- check if duplicates, and if so, throw illegal argument
                          return $ Fix $ ESet evals
        go (EHM pairs) = do evals <- mapMtuple eval pairs
                            -- check if duplicate keys, and if so, throw illegal argument
                            return $ Fix $ EHM evals
        go x = return $ Fix x
        mapMtuple f = mapM (\(x,y) -> (,) <$> f x <*> f y)

ifn :: Expr -> EvalState Fn
ifn = go . unFix
  where go :: SwjExp Expr -> EvalState Fn
        go s@(ESym _ _) = lookupThing s
        go kw@(EKw _ _) = lookupThing kw
        go (EFn f) = return f
        go v@(EVec _) = lookup1 v
        go s@(ESet _) = lookup1 s
        go hm@(EHM _) = lookup12 hm
        go _ = throwError $ CastException "some type" "IFn"
        lookupThing s = unnamedPrim
                        (\xs -> let (f, r) = splitAt 1 xs in
                                 getFn $ f ++ [Fix s] ++ r)
        lookup12 hm = unnamedPrim $ getFn . (Fix hm :)
        lookup1 x = unnamedPrim $ get1Fn . (Fix x :)
        unnamedPrim = return . PrimFn . Prim ("", "")

specials :: M.Map String a
specials = M.fromList
           [ ("fn*", undefined)
           , ("quote", undefined)
           , (".", undefined)
           , ("var", undefined)
           , ("&", undefined)
           , ("if", undefined)
           , ("var", undefined)
           ]

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
