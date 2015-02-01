{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Errors where

import Text.ParserCombinators.Parsec (ParseError)

data SwjError = SyntaxError ParseError

errString :: SwjError -> String
errString (SyntaxError err) = show err

