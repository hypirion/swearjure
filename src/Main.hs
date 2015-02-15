{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable (toList)
import           Data.Generics.Fixplate (Mu(..))
import           Data.Sequence
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import           Filesystem hiding (readFile, writeFile)
import           Filesystem.Path.CurrentOS
import           Swearjure.AST (prStr, Val, EvalState, SwjValF(EVec), _nil)
import           Swearjure.Errors
import           Swearjure.Eval (initEnv, eval)
import           Swearjure.Reader
import           System.Console.Readline (readline, addHistory)
import           System.Environment
import           System.IO (hIsTerminalDevice, stdin)

main :: IO ()
main = do args <- getArgs
          case args of
           ["-h"] -> banner
           ["-?"] -> banner
           ["--help"] -> banner
           [x] -> do contents <- readFile x
                     static contents
           [] -> do isTerm <- hIsTerminalDevice stdin
                    if isTerm
                      then interactive
                      else do contents <- getContents
                              static contents
           _ -> banner

banner :: IO ()
banner = putStrLn "Swearjure, version (+).(*).(+)-SNAPSHOT (aka 0.1.0-SNAPSHOT)"

static :: String -> IO ()
static input = do let wrap = '[' : input ++ "]"
                  ev <- staticEval wrap
                  case ev of
                   Just s  -> putStrLn s
                   Nothing -> return ()

interactive :: IO ()
interactive = do hist <- readHistory
                 void $ runStateT (loop 1) hist

loop :: Int -> StateT (Seq String) IO ()
loop gsymCount
  = do input <- liftIO $ readline "swj> "
       case input of
        Nothing -> do get >>= liftIO . writeHistory
                      return ()
        Just str ->
          do liftIO $ addHistory str
             modify (|> str)
             (res, symCount) <- liftIO $ re gsymCount str
             case res of
              Just x -> liftIO $ putStrLn x
              Nothing -> return ()
             loop symCount

re :: Int -> String -> IO (Maybe String, Int)
re n s = do res <- runExceptT (runStateT (runReaderT (maybeEval s) initEnv) n)
            case res of
             Left err -> return (Just $ errString err, n)
             Right (Just x, n') -> return (Just $ prStr x, n')
             Right (Nothing, n') -> return (Nothing, n')

-- return last element of the vec
staticEval :: String -> IO (Maybe String)
staticEval s = do res <- runExceptT (runStateT (runReaderT (maybeEval s) initEnv) 1)
                  case res of
                   Left err -> return $ Just $ errString err
                   Right (Just (Fix (EVec xs)), _)
                     | last xs /= _nil -> return $ Just $ prStr (last xs)
                   Right (Just _, _) -> return Nothing
                   Right (Nothing, _) -> return Nothing

maybeEval :: String -> EvalState (Maybe Val)
maybeEval s = readVal s >>= T.mapM eval

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
