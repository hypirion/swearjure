{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Reader where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Generics.Fixplate as F
import           Data.Generics.Fixplate hiding (mapM)
import           Data.List (elemIndex)
import qualified Data.Map as M
import           Data.Maybe (maybeToList, fromMaybe, isJust)
import           Prelude hiding (seq)
import           Swearjure.AST
import           Swearjure.Errors
import           Swearjure.Parser

readVal :: PVal -> EvalState Val
readVal ast = replaceFnLits ast >>= convertAst

convertAst :: PVal -> EvalState Val
convertAst = go
  where go :: PVal -> EvalState Val
        go = liftM Fix . goF . unFix
        goF :: PValF PVal -> EvalState (SwjValF Val)
        goF (PSym s) = return $ uncurry ESym $ splitSym s
        goF (PString s) = return $ EStr s
        goF (PKw s) = return $ uncurry EKw $ splitSym s
        goF (PQualKw s) = return $ EKw (Just "user") s
        -- -^ if we could move nses, this would've forced this fn to be of
        -- EvalState
        goF (PChar c) = return $ EChar c
        -- TODO: Go over to compdata. Excellent fit here actually, as this
        -- traversal is over a desugared ast.
        goF (PFnLit _) = throwError $ IllegalState $ "Internal error: Function "
                         ++ "literals should've been eradicated by now"
        goF (PList xs) = EList <$> mapM go xs
        goF (PVec xs) = EVec <$> mapM go xs
        -- randomization of sets and hash maps
        goF (PSet xs) = do vals <- mapM go xs
                           ESet <$> shuffle vals
        -- want this to be Data.Set, but forces eq + ord on Mu/Attr
        goF (PHM pairs) = do vals <- mapM (\(x, y) -> (,) <$> go x <*> go y) pairs
                             EHM <$> shuffle vals
        goF (PSyntaxQuote x) = syntaxUnquote x >>= goF

splitSym :: String -> (Maybe String, String)
splitSym "/" = (Nothing, "/")
splitSym s = case '/' `elemIndex` s of
              Nothing -> (Nothing, s)
              Just idx -> let (ns, name) = splitAt idx s in
                           (Just ns, tail name)

replaceFnLits :: PVal -> EvalState PVal
replaceFnLits (Fix (PFnLit xs'))
  = do xs <- replacePercents xs'
       return $ Fix $ PList $ (Fix $ PSym "fn*") : xs
replaceFnLits (Fix x) = Fix <$> F.mapM replaceFnLits x

replacePercents :: [PVal] -> EvalState [PVal]
replacePercents e = prepareArglist <$> runStateT (mapM (cataM go) e)
                    (Nothing, Nothing)
  where go (PSym "%")
          = do (p1, r) <- get
               case p1 of
                Nothing -> do cnt <- lift get
                              lift $ modify succ
                              let p1s = p1sym cnt
                              put (Just p1s, r)
                              return p1s
                (Just p1s) -> return p1s
        go (PSym "%&")
          = do (p1, r) <- get
               case r of
                Nothing -> do cnt <- lift get
                              lift $ modify succ
                              let rsym = restSym cnt
                              put (p1, Just rsym)
                              return rsym
                (Just rsym) -> return rsym
        go x = return $ Fix x
        p1sym cnt = Fix $ PSym $ "p1__" ++ show cnt ++ "#"
        restSym cnt = Fix $ PSym $ "rest__" ++ show cnt ++ "#"
        prepareArglist (es, (p1, rest))
          = let arglist = maybeToList p1 ++ restArglist rest in
             [ (Fix . PVec) arglist
             , (Fix . PList) es]
        restArglist (Just rest) = [Fix $ PSym "&", rest]
        restArglist Nothing = []

syntaxUnquote :: PVal -> EvalState (PValF PVal)
syntaxUnquote e = fst <$> runStateT (go $ unFix e) M.empty
  where go :: PValF PVal -> StateT (M.Map String (PValF PVal)) EvalState (PValF PVal)
        go sym@(PSym s)
          | last s == '#'
             = do r <- gets (M.lookup s)
                  case r of
                   Just replacement -> return replacement
                   Nothing ->
                     do symCnt <- lift get
                        lift $ modify succ
                        let gsym = PList [_pquote, gensym s symCnt]
                        modify $ M.insert s gsym
                        return gsym
          | last s == '.' = throwError $ IllegalState "expansion of class ctors not implemented yet"
          | head s == '.' = return $ PList [_pquote, Fix sym]
          | isJust $ fst (splitSym s) = return $ PList [_pquote, Fix sym]
          | otherwise = do newSym <- fromMaybe (Fix $ PSym $ "user/" ++ s)
                                     <$> liftM (fmap $ Fix . deSym . unFix)
                                         (getMapping s)
                           return $ PList [_pquote, newSym]
        go lst@(PList []) = return lst
        go (PList xs)
          | head xs == _punquote = return $ unFix $ xs !! 1
          | head xs == _punquoteSplicing
               = throwError $ IllegalState "splice not in list"
          | otherwise = do seq <- sqExpand xs
                           return $ PList [_pseq, iPList $ _pconcat : seq]
        go (PVec xs)
          = do seq <- sqExpand xs
               return $ PList [_papply, _pvector,
                               iPList [_pseq, iPList $ _pconcat : seq]]
        go (PSet xs)
          = do seq <- sqExpand xs
               return $ PList [_papply, _phashset,
                               iPList [_pseq, iPList $ _pconcat : seq]]
        go (PHM pairs)
          = do seq <- sqExpand (unpair pairs)
               return $ PList [_papply, _phashmap,
                               iPList [_pseq, iPList $ _pconcat : seq]]
        go x = return x
        sqExpand = mapM (listGo . unFix)
        listGo lst@(PList []) = return $ iPList [_plist, Fix lst]
        listGo ls@(PList xs)
          | head xs == _punquote = return $ iPList [_plist, xs !! 1]
          | head xs == _punquoteSplicing
               = return $ xs !! 1
          | otherwise = do squote <- go ls
                           return $ iPList [_plist, Fix squote]
        listGo x = do squote <- go x
                      return $ iPList [_plist, Fix squote]
        unpair = concatMap (\(x,y) -> [x, y])
        gensym s cnt = Fix $ PSym $ init s ++ "__"
                       ++ show cnt ++ "__auto__"
        -- TODO: This is hacky.
        _pquote = Fix $ PSym "quote"
        _punquote = Fix $ PSym "clojure.core/unquote"
        _punquoteSplicing = Fix $ PSym "clojure.core/unquote-splicing"
        _pconcat = Fix $ PSym "clojure.core/concat"
        _papply = Fix $ PSym "clojure.core/apply"
        _pvector = Fix $ PSym "clojure.core/vector"
        _phashmap = Fix $ PSym "clojure.core/hash-map"
        _phashset = Fix $ PSym "clojure.core/hash-set"
        _plist = Fix $ PSym "clojure.core/list"
        _pseq =  Fix $ PSym "clojure.core/seq"
        deSym (ESym a b) = PSym $ maybe "" (++ "/") a ++ b
        deSym _ = error "getMapping returned non-sym back"
        iPList = Fix . PList
