{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Reader where

import           Control.Applicative ((<$>))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Generics.Fixplate hiding (mapM)
--import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Swearjure.AST
import           Swearjure.Errors
import           Swearjure.Eval
import           Swearjure.Parser

readExpr :: String -> StateT Int (Except SwjError) (Maybe Expr)
readExpr str = (lift $ readAst str) >>= convertAst

convertAst :: Maybe PVal -> StateT Int (Except SwjError) (Maybe Expr)
convertAst Nothing = return Nothing
convertAst (Just ast) = Just <$> cataM (liftM Fix . go) ast
  where go (PSym s) = return $ ESym Nothing s -- nil check here
        go (PString s) = return $ EStr s
        go (PKw s) = return $ EKw Nothing s
        go (PQualKw s) = return $ EKw (Just "user") s
        -- -^ if we could move nses, this would've forced this fn to be of
        -- EvalState
        go (PChar c) = return $ EChar c
        go (PFnLit xs) = do cnt <- get
                            let (xs', cnt') = replaceFnLits cnt xs
                            put cnt'
                            return $ EList $ fnStar : xs'
        go (PList xs) = return $ EList xs
        go (PVec xs) = return $ EVec xs
        go (PSet xs) = return $ ESet xs
        -- want this to be Data.Set, but forces eq + ord on Mu/Attr
        go (PHM pairs) = return $ EHM pairs -- Same here, ugh.
        go (PSyntaxQuote x) = unFix <$> syntaxUnquote x
        fnStar = (Fix . ESym Nothing) "fn*"

-- returns expression plus arglist, along with symcount
replaceFnLits :: Int -> [Expr] -> ([Expr], Int)
replaceFnLits n e = prepareArglist
                    $ runState (mapM (cataM go) e)
                               (n, Nothing, Nothing)
  where go (ESym Nothing "%")
          = do (cnt, p1, r) <- get
               case p1 of
                Nothing -> do let p1s = p1sym cnt
                              put (cnt + 1, Just p1s, r)
                              return p1s
                (Just p1s) -> return p1s
        go (ESym Nothing "%&")
          = do (cnt, p1, r) <- get
               case r of
                Nothing -> do let rsym = restSym cnt
                              put (cnt + 1, p1, Just rsym)
                              return rsym
                (Just rsym) -> return rsym
        go x = return $ Fix x
        p1sym cnt = Fix $ ESym Nothing $ "p1__" ++ show cnt ++ "#"
        restSym cnt = Fix $ ESym Nothing $ "rest__" ++ show cnt ++ "#"
        prepareArglist (es, (cnt, p1, rest))
          = let arglist = maybeToList p1 ++ restArglist rest in
             ([ (Fix . EVec) arglist
              , (Fix . EList) es],
              cnt)
        restArglist (Just rest) = [Fix $ ESym Nothing "&", rest]
        restArglist Nothing = []



syntaxUnquote :: Expr -> StateT Int (Except SwjError) Expr
syntaxUnquote = go . unFix -- need some state handling here dawg.
  where go sym@(ESym Nothing s)
          | last s == '#' = throwError $ IllegalState "auto-gensyms not implemented"
          | last s == '.' = throwError $ IllegalState "expansion of "
          | head s == '.' = return $ iList [_quote, Fix sym]
          | otherwise = return $ Fix sym
        -- ^ do some lookup here to expand. Needs a ReaderT
        go (EList xs)
          | head xs == _unquote = return (xs !! 1)
          | head xs == _unquoteSplicing
               = throwError $ IllegalState "splice not in list"
          | otherwise = return $ iList [_seq, iList $ _concat : sqExpand xs]
        go (EVec xs)
          = return $ iList [_apply, _vector,
                            iList [_seq, iList $ _concat : sqExpand xs]]
        go (ESet xs)
          = return $ iList [_apply, _hashset,
                            iList [_seq, iList $ _concat : sqExpand xs]]
        go (EHM pairs)
          = return $ iList [_apply, _hashmap,
                            iList [_seq, iList $ _concat :
                                         sqExpand (unpair pairs)]]
        go x = return $ Fix x
        sqExpand :: [Expr] -> [Expr]
        sqExpand = map (listGo . unFix)
        listGo :: SwjExp Expr -> Expr
        listGo ls@(EList xs)
          | head xs == _unquote = iList [_list, xs !! 1]
          | head xs == _unquoteSplicing
               = xs !! 1
          | otherwise = iList [_list, Fix ls]
        listGo x = Fix x
        unpair = concatMap (\(x,y) -> [x, y])
