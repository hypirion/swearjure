{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable (toList)
import           Data.Sequence
import qualified Data.Sequence as S
import           Filesystem hiding (readFile, writeFile)
import           Filesystem.Path.CurrentOS
import           Swearjure.AST (prStr, Expr, EvalState)
import           Swearjure.Errors
import           Swearjure.Eval (initEnv, eval)
import           Swearjure.Reader
import           System.Console.Readline (readline, addHistory)

main :: IO ()
main = do hist <- readHistory
          runStateT (loop 1) hist >> return ()

loop :: Int -> StateT (Seq String) IO ()
loop gsymCount
  = do input <- liftIO $ readline "swj> "
       case input of
        Nothing -> do get >>= liftIO . writeHistory
                      return ()
        Just str ->
          do liftIO $ addHistory str
             modify (|> str)
             let (res, symCount) = (re gsymCount str)
             case res of
              Just x -> liftIO $ putStrLn x
              Nothing -> return ()
             loop symCount

re :: Int -> String -> (Maybe String, Int)
re n s = case runExcept (runStateT (runReaderT (repl s) initEnv) n) of
          Left err -> (Just $ errString err, n)
          Right (Just x, n') -> (Just $ prStr x, n')
          Right (Nothing, n') -> (Nothing, n')

repl :: String -> EvalState (Maybe Expr)
repl s = do ast <- readExpr s
            case ast of
             Just val -> Just <$> eval val
             Nothing -> return Nothing

readHistory :: IO (Seq String)
readHistory = do hdir <- getAppDataDirectory "swearjure"
                 createTree hdir
                 let file = hdir </> fromText "history"
                 isF <- isFile file
                 if isF
                    then do ls <- lines <$> readFile (encodeString file)
                            mapM_ addHistory ls
                            return $ fromList ls
                    else return S.empty

writeHistory :: Seq String -> IO ()
writeHistory hist = do hdir <- getAppDataDirectory "swearjure"
                       createTree hdir
                       let fname = encodeString (hdir </> fromText "history")
                       let hist' = takeLast 1000 hist
                       writeFile fname (unlines $ toList hist')

-- | returns the last n elements in the seq as a seq, or the seq itself if it
-- contains less than n elements.
takeLast :: Int -> Seq a -> Seq a
takeLast n s = S.drop (S.length s - n) s
