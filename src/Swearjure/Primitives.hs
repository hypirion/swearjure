{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RankNTypes #-}

module Swearjure.Primitives where

import           Control.Applicative ((<$>))
import           Control.Monad.Except
import           Data.Char (chr, ord)
import           Data.Generics.Fixplate
import qualified Data.Map as M
import           Data.Ratio
import qualified Data.Set as S
import           Prelude hiding (seq)
import           Swearjure.AST hiding (lookup)
import           Swearjure.Errors
import           System.IO

getFn :: [Val] -> EvalState Val
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

get1Fn :: [Val] -> EvalState Val
get1Fn [m, k] = go (unFix m)
  where go (EVec vals)
          = case unFix k of
             EInt n | n < 0 -> throwError $ IllegalArgument "Index can't be negative"
             EInt n | length vals > fromIntegral n -> return $ vals !! fromIntegral n
             EInt _ -> throwError $ IllegalArgument "Index out of bounds for vector"
             _ -> throwError $ IllegalArgument "Key must be integer"
        go (ESet vals)
          = if k `elem` vals
            then return k
            else return $ Fix Nil
        go _ = throwError $ IllegalState "Internal Swearjure error -- get1Fn got unexpected value"
get1Fn xs = throwError $ ArityException (length xs) (typeName $ head xs)

-- this one must be wrapped properly to use in envs. (liftM (Fix . EList) . seq)
seq :: [Val] -> EvalState [Val]
seq [x] = go (unFix x)
  where go (ESet vals) = return vals
        go (EList vals) = return vals
        go (EVec vals) = return vals
        go (EHM pairs) = return $ vecPairs pairs
        go (EStr s) = return $ map (Fix . EChar) s
        go Nil = return []
        go x' = throwError $ CastException (typeName' x') "ISeq"
        vecPairs = map (\(a, b) -> (Fix (EVec [a, b])))
seq x = throwError $ ArityException (length x) "core/seq"

-- same as above
concat :: [Val] -> EvalState [Val]
concat xs = foldM prepend [] (reverse xs)
  where prepend acc v = do s <- seq [v]
                           return $ s ++ acc

multiCmp :: (SwjValF Val -> SwjValF Val -> EvalState Bool) -> String
            -> [SwjValF Val] -> EvalState Val
multiCmp _ fname [] = throwError $ ArityException 0 fname
multiCmp _ _ [_] = return $ Fix $ EBool True
multiCmp f _ [x, y] = liftM (Fix . EBool) $ f x y
multiCmp f fname (a : b : r) = do res <- f a b
                                  if res
                                    then multiCmp f fname (b : r)
                                    else return $ Fix $ EBool False

numOp :: (forall a. Ord a => a -> a -> Bool) -> SwjValF Val -> SwjValF Val
         -> EvalState Bool
numOp op = cmp
  where cmp (EInt x) (EInt y) = return $ x `op` y
        cmp (EInt x) (EFloat y) = return $ fromIntegral x `op` y
        cmp (EInt x) (ERatio y) = return $ (x % 1) `op` y
        cmp (EFloat x) (EInt y) = return $ x `op` fromIntegral y
        cmp (EFloat x) (EFloat y) = return $ x `op` y
        cmp (EFloat x) (ERatio y) = return $ x `op` asFloat y
        cmp (ERatio x) (ERatio y) = return $ x `op` y
        cmp (ERatio x) (EInt y) = return $ x `op` (y % 1)
        cmp (ERatio x) (EFloat y) = return $ asFloat x `op` y
        cmp x y
          | isNum x = throwError $ CastException (typeName' y) "Number"
          | otherwise = throwError $ CastException (typeName' x) "Number"

lt :: [Val] -> EvalState Val
lt = multiCmp (numOp (<)) "core/<" . map unFix

lte :: [Val] -> EvalState Val
lte = multiCmp (numOp (<=)) "core/<=" . map unFix

gt :: [Val] -> EvalState Val
gt = multiCmp (numOp (>)) "core/>" . map unFix

gte :: [Val] -> EvalState Val
gte = multiCmp (numOp (>=)) "core/>=" . map unFix

numEq :: [Val] -> EvalState Val
numEq = multiCmp (numOp (==)) "core/==" . map unFix

eq :: [Val] -> EvalState Val
eq [] = throwError $ ArityException 0 "core/="
eq [_] = return $ Fix $ EBool True
eq (x : y : r) = if x == y
                 then eq (y : r)
                 else return $ Fix $ EBool False

-- hash-map and hash-set

hashMap :: [Val] -> EvalState Val
hashMap xs = Fix . EHM  <$> (go M.empty xs >>= shuffle . M.toList)
  where go m [] = return m
        go _ [k] = throwError $ IllegalArgument $ "No value supplied for key: " ++ prStr k
        go m (k : v : kvs) = go (M.insert k v m) kvs

hashSet :: [Val] -> EvalState Val
hashSet xs = Fix . ESet <$> (go S.empty xs >>= shuffle . S.toList)
  where go s [] = return s
        go s (v : vs) = go (S.insert v s) vs

-- ->> and ->

threadLast :: [Val] -> EvalState Val
threadLast [] = throwError $ ArityException 0 "core/->>"
threadLast [x] = return x
threadLast (x : Fix (EList ys) : r) = threadLast $ iList (ys ++ [x]) : r
threadLast (x : y : r) = threadLast $ iList [y, x] : r

threadSnd :: [Val] -> EvalState Val
threadSnd [] = throwError $ ArityException 0 "core/->"
threadSnd [x] = return x
threadSnd (x : Fix (EList ys) : r)
  = let (yfst, ysnd) = splitAt 1 ys in
     threadSnd $ iList (yfst ++ [x] ++ ysnd) : r
threadSnd (x : y : r) = threadSnd $ iList [y, x] : r

-- I see that these operations can be generalized, but it won't make them easier
-- to maintain or anything, really.

plus :: [Val] -> EvalState Val
plus xs = Fix <$> foldM (|+|) (EInt 0) (map unFix xs)
  where (EInt x) |+| (EInt y) = return $ EInt (x + y)
        (EInt x) |+| (EFloat y) = return $ EFloat (fromIntegral x + y)
        (EInt x) |+| (ERatio rat) = unRatio ((x % 1) + rat)
        (EFloat x) |+| (EInt y) = return $ EFloat (x + fromIntegral y)
        (EFloat x) |+| (EFloat y) = return $ EFloat (x + y)
        (EFloat x) |+| (ERatio rat) = return $ EFloat (x + asFloat rat)
        (ERatio rat) |+| (EInt y) = unRatio (rat + (y % 1))
        (ERatio rat) |+| (EFloat y) = return $ EFloat (asFloat rat + y)
        (ERatio x) |+| (ERatio y) = unRatio (x + y)
        x |+| y
          | isNum x = throwError $ CastException (typeName' y) "Number"
          | otherwise = throwError $ CastException (typeName' x) "Number"

minus :: [Val] -> EvalState Val
minus [] = throwError $ ArityException 0 "core/-"
minus [x] = minus [Fix (EInt 0), x]
minus (x' : xs) = Fix <$> foldM (|-|) (unFix x') (map unFix xs)
  where (EInt x) |-| (EInt y) = return $ EInt (x - y)
        (EInt x) |-| (EFloat y) = return $ EFloat (fromIntegral x - y)
        (EInt x) |-| (ERatio rat) = unRatio ((x % 1) - rat)
        (EFloat x) |-| (EInt y) = return $ EFloat (x - fromIntegral y)
        (EFloat x) |-| (EFloat y) = return $ EFloat (x - y)
        (EFloat x) |-| (ERatio rat) = return $ EFloat (x - asFloat rat)
        (ERatio rat) |-| (EInt y) = unRatio (rat - (y % 1))
        (ERatio rat) |-| (EFloat y) = return $ EFloat (asFloat rat - y)
        (ERatio x) |-| (ERatio y) = unRatio (x - y)
        x |-| y
          | isNum x = throwError $ CastException (typeName' y) "Number"
          | otherwise = throwError $ CastException (typeName' x) "Number"

mul :: [Val] -> EvalState Val
mul xs = Fix <$> foldM (|*|) (EInt 1) (map unFix xs)
  where (EInt x) |*| (EInt y) = return $ EInt (x * y)
        (EInt x) |*| (EFloat y) = return $ EFloat (fromIntegral x * y)
        (EInt y) |*| (ERatio rat) = unRatio ((y % 1) * rat)
        (EFloat x) |*| (EInt y) = return $ EFloat (x * fromIntegral y)
        (EFloat x) |*| (EFloat y) = return $ EFloat (x * y)
        (EFloat x) |*| (ERatio rat) = return $ EFloat (x * asFloat rat)
        (ERatio rat) |*| (EInt y) = unRatio (rat * (y % 1))
        (ERatio rat) |*| (EFloat y) = return $ EFloat (asFloat rat * y)
        (ERatio x) |*| (ERatio y) = unRatio (x * y)
        x |*| y
          | isNum x = throwError $ CastException (typeName' y) "Number"
          | otherwise = throwError $ CastException (typeName' x) "Number"

divFn :: [Val] -> EvalState Val
divFn [] = throwError $ ArityException 0 "core//"
divFn [x] = divFn [Fix (EInt 1), x]
divFn (x' : xs) = Fix <$> foldM divide (unFix x') (map unFix xs)
  where (EInt n) `divide` (EInt val) = safeRat n val
        (EInt x) `divide` (ERatio y) = unRatio $ (x % 1) / y
        (EInt x) `divide` (EFloat y) = return $ EFloat (fromIntegral x / y)
        (EFloat x) `divide` (EInt y) = return $ EFloat (x / fromIntegral y)
        (EFloat x) `divide` (EFloat y) = return $ EFloat (x / y)
        (EFloat x) `divide` (ERatio rat) = EFloat x `divide` EFloat (asFloat rat)
        (ERatio _) `divide` (EInt 0) = throwError $ IllegalArgument "Cannot divide by 0"
        (ERatio rat) `divide` (EInt y) = unRatio (rat / (y % 1))
        (ERatio rat) `divide` (EFloat y) = EFloat (asFloat rat) `divide` EFloat y
        (ERatio x) `divide `(ERatio y) = unRatio (x / y)
        x `divide` y
          | isNum x = throwError $ CastException (typeName' y) "Number"
          | otherwise = throwError $ CastException (typeName' x) "Number"

prChars :: [Val] -> EvalState Val
prChars [] = return $ Fix Nil
prChars (Fix (EInt n) : xs) = do liftIO $ putChar (chr $ fromIntegral n)
                                 liftIO $ hFlush stdout
                                 prChars xs
prChars (x : _) = throwError $ CastException (typeName x) "Integer"

readChar :: [a] -> EvalState Val
readChar [] = do n <- ord <$> liftIO getChar
                 return $ Fix $ EInt (toInteger n)
readChar xs = throwError $ ArityException (length xs) "swearjure.core/<<'"

prn :: [Val] -> EvalState Val
prn xs = do let printString = unwords $ map prStr xs
            liftIO $ putStrLn printString
            return $ Fix Nil

safeRat :: Integer -> Integer -> EvalState (SwjValF e)
safeRat num den = case den of
                   0 -> throwError $ IllegalArgument "Cannot divide by 0"
                   _ -> unRatio (num % den)

unRatio :: Rational -> EvalState (SwjValF e)
unRatio rat = return (case denominator rat of
                       1 -> EInt $ numerator rat
                       _ -> ERatio rat)

asFloat :: Rational -> Double
asFloat rat = fromIntegral (numerator rat) / fromIntegral (denominator rat)

isNum :: SwjValF e -> Bool
isNum (EInt _) = True
isNum (EFloat _) = True
isNum (ERatio _) = True
isNum _ = False
