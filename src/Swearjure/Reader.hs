{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Reader where

import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Fixplate hiding (mapM)
import Data.Maybe (maybeToList)
import Swearjure.AST
import Swearjure.Errors
import Swearjure.Parser

readExpr :: String -> StateT Int (Except SwjError) (Maybe Expr)
readExpr str = (lift $ readAst str) >>= convertAst

convertAst :: Maybe PVal -> StateT Int (Except SwjError) (Maybe Expr)
convertAst Nothing = return Nothing
convertAst (Just ast) = liftM Just $ cataM (liftM Fix . go) ast
  where go (PSym s) = return $ ESym Nothing s
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
