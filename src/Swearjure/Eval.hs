{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Eval where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Generics.Fixplate (Mu(..))
import           Data.List (nub, sortBy)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Traversable as T
import           Prelude hiding (lookup, seq, concat)
import           Swearjure.AST
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
          ++ map
          (\(fname, f) -> (fname, ("swearjure.core",
                                   (False, Fix $ EFn $ PrimFn
                                           (Prim ("swearjure.core", fname) f)))))
          [ ("<<'", readChar)
          , (">>'", prChars)
          , (">>", prn)
          ]

apply :: [Val] -> EvalState Val
apply [] = throwError $ ArityException 0 "core/apply"
apply [_] = throwError $ ArityException 1 "core/apply"
apply (f : xs) = do fn <- ifn f
                    lastOnes <- seq [last xs]
                    let spliced = init xs ++ lastOnes
                    runFn fn spliced

runFn :: Fn -> [Val]-> EvalState Val
runFn f@Fn {fnEnv = env, fnRecName = fname
           , fnFns = options} args
  = do (paramNames, restName, exprs) <- findOption (length args) options
       mapping <- prepMapping paramNames restName args $ recBinding fname
       extendEnv env mapping (evalAll exprs)
    where findOption n [] = throwError $ ArityException n (prStr $ Fix $ EFn f)
          findOption n (opt : rst)
            | argcPred opt n = return opt
            | otherwise = findOption n rst
          argcPred (params, Just _, _) = (length params <=)
          argcPred (params, Nothing, _) = (length params ==)
          recBinding (Just x) = [(x, Fix $ EFn f)]
          recBinding Nothing = []
          prepMapping (p : ps) r (x : xs) acc = prepMapping ps r xs $ (p, x) : acc
          prepMapping [] (Just r) xs acc = return $ (r, Fix $ EList xs) : acc
          prepMapping [] Nothing [] acc = return acc
          prepMapping _ _ _ _ = throwError $ IllegalState $ "Well, we still have"
                              ++ " some arguments left, but no values to assign"
          evalAll exprs = last <$> mapM eval exprs
runFn (PrimFn (Prim _ prim)) args = prim args

-- For later: do some tail recursion tricks.

macroexpand :: Val -> EvalState Val
macroexpand lst@(Fix (EList (x@(Fix (ESym _ s)) : xs)))
  | x == _quote = return lst
  | x == _fnStar = return lst
  | otherwise = do maybeMacro <- lookupMacro s
                   case maybeMacro of
                    Just macro ->
                      do expandOnce <- apply [macro, (Fix (EList xs))]
                         macroexpand expandOnce
                    Nothing -> Fix <$> T.mapM macroexpand (unFix lst)
macroexpand (Fix x) = Fix <$> T.mapM macroexpand x

eval :: Val -> EvalState Val
eval = macroexpand >=> go . unFix
  where go (ESym _ s) = lookup s
        go x@(EList []) = return $ Fix x
        go (EList xs)
          | head xs == _quote = return $ fromMaybe _nil (listToMaybe $ tail xs)
          | head xs == _nil = throwError $ IllegalArgument "Can't call nil"
          | head xs == _fnStar = makeLambda $ tail xs
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

-- from GHC.Exts

-- | The 'sortWith' function sorts a list of elements using the
-- user supplied function to project something out of each element
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

makeLambda :: [Val] -> EvalState Val
makeLambda xs = do env <- ask
                   num <- get
                   modify (+2)
                   (recName, rst) <- findName $ map unFix xs
                   fns <- sortWith (\(x, y, _) -> length x + countMaybe y)
                          <$> mkUnsureFn rst
                   validateArity $ map (\(x, y, _) -> (length x, y)) fns
                   -- TODO: Walk the values and verify that symbols and vars can
                   -- be resolved.
                   return $ Fix $ EFn $
                     Fn { fnEnv = env
                        , fnNs = "user"
                        , fnName = mungedName recName num
                        , fnRecName = recName
                        , fnFns = fns
                        }
  where mungedName (Just fname) num = "eval" ++ show num ++ "$" ++ fname ++ "__"
                                      ++ show (num + 1)
        mungedName Nothing num = mungedName (Just "fn") num
        findName ((ESym _ s) : rst)
          | S.member s specials = throwError $ IllegalArgument
                                  $ "Can't use " ++ s ++ " as function name"
          | otherwise = return (Just s, rst)
        findName r = return (Nothing, r)
        mkUnsureFn :: [SwjValF Val] -> EvalState [([String], Maybe String, [Val])]
        mkUnsureFn [] = return []
        mkUnsureFn lst@((EList _) : _) = mkFns lst
        mkUnsureFn lst@((EVec _) : _) = do fn <- mkFn lst
                                           return [fn]
        mkUnsureFn (x : _) = throwError $ CastException (typeName' x) "List/Vector"
        mkFn ((EVec args') : exprs)
          = do (args, restArg) <- validateArgs $ map unFix args'
               return (args, restArg, map Fix exprs)
        mkFn (x : _) = throwError $ CastException (typeName' x) "Vector"
        mkFn [] = throwError $ IllegalArgument
                  "(This gives a NullPointerException in Clojure)"
        mkFns = mapM (mkFn <=< unList)
        unList (EList ys) = return $ map unFix ys
        unList x = throwError $ CastException (typeName' x) "List"
        validateArgs [] = return ([], Nothing)
        validateArgs (s@(ESym (Just _) _) : _)
          = throwError $ IllegalArgument $
            "Can't use qualified name as parameter: " ++ prStr (Fix s)
        validateArgs [(ESym Nothing "&"), s@(ESym (Just _) _)]
          = throwError $ IllegalArgument $
            "Can't use qualified name as parameter: " ++ prStr (Fix s)
        validateArgs [(ESym Nothing "&"), (ESym Nothing s)]
          | s /= "&" = return ([], Just s)
          | otherwise = throwError $ IllegalArgument "Invalid parameter list"
        validateArgs ((ESym Nothing s) : args')
          | s /= "&" = do (args, restArg) <- validateArgs args'
                          return (s : args, restArg)
          | otherwise = throwError $ IllegalArgument "Invalid parameter list"
        validateArgs (x : _) = throwError $ IllegalArgument
                               $ "Unsupported binding key: " ++ prStr (Fix x)
        -- assumes these are sorted by size
        validateArity ys
          = do when (1 < variadicCount ys) $
                 throwError $ IllegalArgument
                 "Can't have more than 1 variadic overload"
               when (ys /= nub ys) $ throwError $ IllegalArgument
                 "Can't have 2 overloads with same arity"
               when (variadicCount ys == 1 &&
                     snd (last ys) == Nothing) $
                 throwError $ IllegalArgument $ "Can't have fixed arity "
                 ++ "function with more params than variadic function"
        variadicCount ys = length (filter isJust (map snd ys))
        countMaybe Nothing = 0
        countMaybe (Just _) = 1

ifn :: Val -> EvalState Fn
ifn = go . unFix
  where go :: SwjValF Val -> EvalState Fn
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

checkDupe :: [Val] -> EvalState ()
checkDupe xs = go S.empty xs
  where go _ [] = return ()
        go s (x : r)
          | S.member x s = throwError $ IllegalState
                           $ "Duplicate entry: " ++ prStr x
          | otherwise = go (S.insert x s) r
