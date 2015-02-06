{-# OPTIONS_GHC -Wall -Werror #-}

module Swearjure.Errors where

import Text.ParserCombinators.Parsec (ParseError)

data SwjError = SyntaxError ParseError
              | IllegalState String

errString :: SwjError -> String
errString (SyntaxError err) = show err
errString (IllegalState err) = show $ "Illegal State: " ++ err

