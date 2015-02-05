{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Swearjure.AST (prStr)
import Swearjure.Errors
import Swearjure.Reader
import System.IO
import System.IO.Error

main :: IO ()
main = loop 1

loop :: Int -> IO ()
loop gsymCount
  = do putStr "swj> "
       hFlush stdout
       input <- try (getLine)
       case input of
        Left e ->
          if isEOFError e
             then return ()
             else ioError e
        Right str ->
          do let (res, symCount) = (re gsymCount str)
             case res of
              Just x -> putStrLn x
              Nothing -> return ()
             loop symCount


re :: Int -> String -> (Maybe String, Int)
re n s = case runExcept (runStateT (readExpr s) n) of
          Left err -> (Just $ errString err, n)
          Right (Just x, n') -> (Just $ prStr x, n')
          Right (Nothing, n') -> (Nothing, n')
