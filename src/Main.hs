{-# OPTIONS_GHC -Wall -Werror #-}

import System.IO
import System.IO.Error
import Control.Exception
import Swearjure.Parser (readExpr)
import Swearjure.Errors

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
         Right (Just ast) -> Just $ show ast
         Right Nothing -> Nothing

