{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Eval where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Except
import           Data.Generics.Fixplate (Mu(..))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import qualified Data.Traversable as T
import           Prelude hiding (lookup, seq, concat)
import           Swearjure.AST hiding (specials)
import           Swearjure.Errors
import           Swearjure.Primitives

initEnv :: Env
initEnv = Toplevel $ M.fromList $ map
          (\(fname, f) -> (fname, ("clojure.core",
                                   (False, Fix $ EFn $ PrimFn
                                           (Prim ("core", fname) f)))))
          [ ("+", plus)
          , ("/", divFn)
          , ("*", mul)
          , ("-", minus)
          , ("list", return . Fix . EList)
          , ("vector", return . Fix . EVec)
          , ("apply", apply)
          , ("seq", liftM (Fix . EList) . seq)
          , ("concat", liftM (Fix . EList) . concat)
          , ("hash-map", hashMap)
          , ("hash-set", hashSet)
          , ("<", lt)
          , (">", gt)
          , ("<=", lte)
          , (">=", gte)
          , ("=", eq)
          , ("==", numEq)
          ] ++ map
          (\(fname, f) -> (fname, ("clojure.core",
                                   (True, Fix $ EFn $ PrimFn
                                          (Prim ("core", fname) f)))))
          -- TODO: Implement these as non-prim macros in Swearjure. Somehow.
          [ ("->>", threadLast)
          , ("->", threadSnd)
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

macroexpand :: Expr -> EvalState Expr
macroexpand lst@(Fix (EList (x@(Fix (ESym _ s)) : xs)))
  | x == _quote = return lst
  | otherwise = do maybeMacro <- lookupMacro s
                   case maybeMacro of
                    Just macro ->
                      do expandOnce <- apply [macro, (Fix (EList xs))]
                         macroexpand expandOnce
                    Nothing -> Fix <$> T.mapM macroexpand (unFix lst)
macroexpand (Fix x) = Fix <$> T.mapM macroexpand x

eval :: Expr -> EvalState Expr
eval = macroexpand >=> go . unFix
  where go (ESym _ s) = lookup s
        go x@(EList []) = return $ Fix x
        go (EList xs)
          | head xs == _quote = return $ fromMaybe _nil (listToMaybe $ tail xs)
          | head xs == _nil = throwError $ IllegalArgument "Can't call nil"
          | otherwise = do f : xs' <- mapM eval xs
                           apply [f, (Fix (EList xs'))]
        go v@(EVec _) = Fix <$> T.mapM eval v
        go (ESet xs) = do evals <- mapM eval xs
                          checkDupe evals
                          return $ Fix $ ESet evals
        go (EHM pairs) = do evals <- mapMtuple eval pairs
                            checkDupe $ map fst evals
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
        go x = throwError $ CastException (typeName' x) "IFn"
        lookupThing s = unnamedPrim
                        (\xs -> let (f, r) = splitAt 1 xs in
                                 getFn $ f ++ [Fix s] ++ r)
        lookup12 hm = unnamedPrim $ getFn . (Fix hm :)
        lookup1 x = unnamedPrim $ get1Fn . (Fix x :)
        unnamedPrim = return . PrimFn . Prim ("", "")

checkDupe :: [Expr] -> EvalState ()
checkDupe xs = go S.empty xs
  where go _ [] = return ()
        go s (x : r)
          | S.member x s = throwError $ IllegalState
                           $ "Duplicate entry: " ++ prStr x
          | otherwise = go (S.insert x s) r
