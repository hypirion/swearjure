{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable (toList)
import           Data.Sequence
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as B
import           Filesystem hiding (readFile, writeFile)
import           Filesystem.Path.CurrentOS
import           Swearjure.AST (prStr, Val, EvalState, _nil)
import           Swearjure.Errors
import           Swearjure.Eval (initEnv, eval)
import           Swearjure.Parser
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
banner = putStrLn "Swearjure, version (*).(+).(+) (aka 1.0.0)"

-- Since attoparsec
static :: String -> IO ()
static input = staticRep 1 (readStatic $ B.pack input)

staticRep :: Int -> [Either SwjError PVal] -> IO ()
staticRep _ [] = return ()
staticRep n [x]
  = do res <- staticEval n x
       case res of
        Left err -> putStrLn $ errString err
        Right (v, _) | v == _nil -> return ()
        Right (v, _) -> putStrLn (prStr v)
staticRep n (x : xs)
  = do res <- staticEval n x
       case res of
        Left err -> putStrLn $ errString err
        Right (_, n') -> staticRep n' xs

staticEval :: Int -> Either SwjError PVal -> IO (Either SwjError (Val, Int))
staticEval n (Right pval) = runExceptT (runStateT (runReaderT (doEval pval) initEnv) n)
staticEval _ (Left err) = return $ Left err

interactive :: IO ()
interactive = do hist <- readHistory
                 void $ runStateT (loop 1) hist

teardown :: StateT (Seq String) IO ()
teardown = get >>= liftIO . writeHistory >> return ()

loop :: Int -> StateT (Seq String) IO ()
loop gsymCount
  = do input <- liftIO $ readline "swj> "
       case input of
        Nothing -> teardown
        Just str ->
          do newGsym <- feedLoop gsymCount (readAsts str)
             case newGsym of
              Nothing -> teardown
              Just val -> loop val

-- returns nothing if we pass in C-d
feedLoop :: Int -> ParseResult -> StateT (Seq String) IO (Maybe Int)
feedLoop gsymCount (Results rseq str)
  = do liftIO $ addHistory str
       modify (|> str)
       n' <- liftIO $ foldM rep gsymCount $ toList rseq
       return $ Just n'
feedLoop gsymCount (Continuation rseq accum cont)
  = do input <- liftIO $ readline "#_=> "
       case input of
        Nothing -> return Nothing
        Just str -> feedLoop gsymCount $ feedCont str rseq accum cont

rep :: Int -> Either SwjError PVal -> IO Int
rep n (Left err) = do putStrLn $ errString err
                      return n
rep n (Right pval)
  = do res <- runExceptT (runStateT (runReaderT (doEval pval) initEnv) n)
       case res of
        Left err -> do putStrLn $ errString err
                       return n
        Right (x, n') -> do putStrLn $ prStr x
                            return n'

doEval :: PVal -> EvalState Val
doEval pval = readVal pval >>= eval

readHistory :: IO (Seq String)
readHistory = do hdir <- getAppDataDirectory "swearjure"
                 createTree hdir
                 let file = hdir </> fromText "history"
                 isF <- isFile file
                 if isF
                    then do ls <- lines0 <$> readFile (encodeString file)
                            mapM_ addHistory ls
                            return $ fromList ls
                    else return S.empty

writeHistory :: Seq String -> IO ()
writeHistory hist = do hdir <- getAppDataDirectory "swearjure"
                       createTree hdir
                       let fname = encodeString (hdir </> fromText "history")
                       let hist' = takeLast 1000 hist
                       writeFile fname (unlines0 $ toList hist')

-- | returns the last n elements in the seq as a seq, or the seq itself if it
-- contains less than n elements.
takeLast :: Int -> Seq a -> Seq a
takeLast n s = S.drop (S.length s - n) s

-- | like unlines, but separates with a null character. Removes the last line.
unlines0 :: [String] -> String
unlines0 = init . concatMap (++ "\0")

-- | line lines, but splits at null characters instead
lines0 :: String -> [String]
lines0 "" = []
lines0 s = cons (case break (== '\0') s of
                  (l, s') -> (l, case s' of
                                  []      -> []
                                  _:s''   -> lines0 s''))
  where
    cons ~(h, t) =  h : t
