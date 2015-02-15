{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Reader where

import           Control.Applicative ((<$>))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Generics.Fixplate hiding (mapM)
import           Data.List (elemIndex)
import qualified Data.Map as M
import           Data.Maybe (maybeToList, fromMaybe)
import           Prelude hiding (seq)
import           Swearjure.AST
import           Swearjure.Errors
import           Swearjure.Parser

readVal :: String -> EvalState (Maybe Val)
readVal str = readAst str >>= convertAst

convertAst :: Maybe PVal -> EvalState (Maybe Val)
convertAst Nothing = return Nothing
convertAst (Just ast) = Just <$> cataM (liftM Fix . go) ast
  where go (PSym s) = return $ uncurry ESym $ splitSym s
        go (PString s) = return $ EStr s
        go (PKw s) = return $ uncurry EKw $ splitSym s
        go (PQualKw s) = return $ EKw (Just "user") s
        -- -^ if we could move nses, this would've forced this fn to be of
        -- EvalState
        go (PChar c) = return $ EChar c
        go (PFnLit xs) = do xs' <- replaceFnLits xs
                            return $ EList $ _fnStar : xs'
        go (PList xs) = return $ EList xs
        go (PVec xs) = return $ EVec xs
        go (PSet xs) = return $ ESet xs
        -- want this to be Data.Set, but forces eq + ord on Mu/Attr
        go (PHM pairs) = return $ EHM pairs -- Same here, ugh.
        -- this one should be ran after pfnlit expansion, otherwise we don't
        -- get correct behaviour when syntax-quoting inside fns :( I think I
        -- might need to go away from Fixplate and catas, and do stuff preorder
        -- instead.
        go (PSyntaxQuote x) = unFix <$> syntaxUnquote x

splitSym :: String -> (Maybe String, String)
splitSym "/" = (Nothing, "/")
splitSym s = case '/' `elemIndex` s of
              Nothing -> (Nothing, s)
              Just idx -> let (ns, name) = splitAt idx s in
                           (Just ns, tail name)

replaceFnLits :: [Val] -> EvalState [Val]
replaceFnLits e = prepareArglist <$> runStateT (mapM (cataM go) e)
                    (Nothing, Nothing)
  where go (ESym Nothing "%")
          = do (p1, r) <- get
               case p1 of
                Nothing -> do cnt <- lift get
                              lift $ modify succ
                              let p1s = p1sym cnt
                              put (Just p1s, r)
                              return p1s
                (Just p1s) -> return p1s
        go (ESym Nothing "%&")
          = do (p1, r) <- get
               case r of
                Nothing -> do cnt <- lift get
                              lift $ modify succ
                              let rsym = restSym cnt
                              put (p1, Just rsym)
                              return rsym
                (Just rsym) -> return rsym
        go x = return $ Fix x
        p1sym cnt = Fix $ ESym Nothing $ "p1__" ++ show cnt ++ "#"
        restSym cnt = Fix $ ESym Nothing $ "rest__" ++ show cnt ++ "#"
        prepareArglist (es, (p1, rest))
          = let arglist = maybeToList p1 ++ restArglist rest in
             [ (Fix . EVec) arglist
             , (Fix . EList) es]
        restArglist (Just rest) = [Fix $ ESym Nothing "&", rest]
        restArglist Nothing = []

syntaxUnquote :: Val -> EvalState Val
syntaxUnquote e = fst <$> runStateT (go $ unFix e) M.empty
  where go sym@(ESym Nothing s)
          | last s == '#'
             = do r <- gets (M.lookup s)
                  case r of
                   Just replacement -> return replacement
                   Nothing ->
                     do symCnt <- lift get
                        lift $ modify succ
                        let gsym = iList [_quote, gensym s symCnt]
                        modify $ M.insert s gsym
                        return gsym
          | last s == '.' = throwError $ IllegalState "expansion of class ctors not implemented yet"
          | head s == '.' = return $ iList [_quote, Fix sym]
          | otherwise = do newSym <- fromMaybe (Fix $ ESym (Just "user") s)
                                     <$> getMapping s
                           return $ iList [_quote, newSym]
        go sym@(ESym _ _) = return $ iList [_quote, Fix sym]
        go lst@(EList []) = return $ Fix lst
        go (EList xs)
          | head xs == _unquote = return (xs !! 1)
          | head xs == _unquoteSplicing
               = throwError $ IllegalState "splice not in list"
          | otherwise = do seq <- sqExpand xs
                           return $ iList [_seq, iList $ _concat : seq]
        go (EVec xs)
          = do seq <- sqExpand xs
               return $ iList [_apply, _vector,
                               iList [_seq, iList $ _concat : seq]]
        go (ESet xs)
          = do seq <- sqExpand xs
               return $ iList [_apply, _hashset,
                               iList [_seq, iList $ _concat : seq]]
        go (EHM pairs)
          = do seq <- sqExpand (unpair pairs)
               return $ iList [_apply, _hashmap,
                               iList [_seq, iList $ _concat : seq]]
        go x = return $ Fix x
        sqExpand = mapM (listGo . unFix)
        listGo lst@(EList []) = return $ iList [_list, Fix lst]
        listGo ls@(EList xs)
          | head xs == _unquote = return $ iList [_list, xs !! 1]
          | head xs == _unquoteSplicing
               = return $ xs !! 1
          | otherwise = do squote <- go ls
                           return $ iList [_list, squote]
        listGo x = do squote <- go x
                      return $ iList [_list, squote]
        unpair = concatMap (\(x,y) -> [x, y])
        gensym s cnt = Fix $ ESym Nothing $ init s ++ "__"
                       ++ show cnt ++ "__auto__"
