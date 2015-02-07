{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Errors where

import Text.ParserCombinators.Parsec (ParseError)

data SwjError = SyntaxError ParseError
              | IllegalState String
              | NotFound String

errString :: SwjError -> String
errString (SyntaxError err) = show err
errString (IllegalState err) = "Illegal State: " ++ err
errString (NotFound sym) = "Unable to resolve symbol: " ++ sym
                           ++ " in this context"

