{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Exception
import Swearjure.Errors
import Swearjure.Reader
import Swearjure.AST (prStr)
import System.IO
import System.IO.Error

main :: IO ()
main = do putStr "swj> "
          hFlush stdout
          input <- try (getLine)
          case input of
           Left e ->
             if isEOFError e
                then return ()
                else ioError e
           Right str ->
             do case (rep str) of
                 Just x -> putStrLn x
                 Nothing -> return ()
                main

rep :: String -> Maybe String
rep s = case readExpr s of
         Left err -> Just $ errString err
         Right (Just ast) -> Just $ prStr ast
         Right Nothing -> Nothing
