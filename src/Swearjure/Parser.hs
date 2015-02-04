{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Swearjure.Parser
       ( ParseVal(..)
       , PVal
       , readAst
       ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Except
import Data.Char (isAlphaNum)
import Data.Foldable (Foldable)
import Data.Generics.Fixplate (Mu(..), ShowF(..))
import Data.Traversable (Traversable)
import Swearjure.Errors
import Text.ParserCombinators.Parsec

type SwjParser = GenParser Char Bool

data ParseVal p = PSym String -- qualified syms. Gur
                | PString String
                | PKw String
                | PQualKw String
                | PChar Char
                | PFnLit [p]
                | PList [p]
                | PVec [p]
                | PSet [p] -- Sharpie!
                | PHM [(p, p)]
                deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type PVal = Mu ParseVal

instance ShowF ParseVal where showsPrecF = showsPrec

cljns :: String
cljns = "clojure.core/"

-- | Discards the result of the parser
omit :: SwjParser p -> SwjParser ()
omit p = p >> return ()

-- | Semi matches a semicolon and returns
semi :: SwjParser Char
semi = char ';'

commentLine :: SwjParser String
commentLine = skipMany1 semi >> manyTill anyChar (omit newline <|> eof)

-- | Filters out any kind of whitespace.
whiteSpace :: SwjParser ()
whiteSpace = skipMany (omit (char ',') <|> omit space <|> omit commentLine
                       <|> try sharpWhites)

sharpWhites :: SwjParser ()
sharpWhites = char '#' >> (sharp_ <|> sharpBang)

sharp_ :: SwjParser ()
sharp_ = lexeme (char '_') >> omit expr

sharpBang :: SwjParser ()
sharpBang = char '!' >> manyTill anyChar (omit newline <|> eof) >> return ()

lexeme :: SwjParser p -> SwjParser p
lexeme p = do x <- p
              whiteSpace
              return x

-- fixme: These are.. nasty to do properly.
startSymChar :: SwjParser Char
startSymChar = oneOf "+-*/!=<>?_&$%"

symChar :: SwjParser Char
symChar = startSymChar <|> oneOf "#'"

symString :: SwjParser String
symString = do x <- startSymChar
               xs <- many symChar
               return $ x : xs

symbol :: SwjParser PVal
symbol = (Fix . PSym)  <$> lexeme symString

-- TODO: qualified kws
keyword :: SwjParser PVal
keyword = Fix . PKw <$> lexeme (char ':' >> symString)

malString :: SwjParser PVal
malString = Fix . PString <$> (lexeme $ between (char '"') (char '"')
                               (many stringChar))
  where stringChar = stringLetter <|> stringEscape
                     <?> "string character (non-alphanumeric)"
        stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (not $ isAlphaNum c))
        -- Todo: Fix this properly?
        stringEscape = char '\\' >> oneOf "\\\""

delimited :: Char -> Char -> SwjParser c -> SwjParser c
delimited start stop = between (lexeme $ char start) (lexeme $ char stop)

list :: SwjParser PVal
list = Fix . PList <$> delimited '(' ')' (many expr)

vec :: SwjParser PVal
vec = Fix . PVec <$> delimited '[' ']' (many expr)

hashMap :: SwjParser PVal
hashMap = Fix . PHM <$> delimited '{' '}' (many pair)
  where pair = (,) <$> expr <*> expr

call :: String -> [PVal] -> PVal
call s es = Fix . PList $ (Fix . PSym) s : es

sugared :: Char -> String -> SwjParser PVal
sugared c s = do (omit . lexeme . char) c
                 e <- expr
                 return $ call (cljns ++ s) [e]

quote :: SwjParser PVal
quote = sugared '\'' "quote"

-- Alright, this is obv. wrong. Need to look into the reader properly, because
-- it does some fancy read time replacement.
backquote :: SwjParser PVal
backquote = sugared '`' "quote"

deref :: SwjParser PVal
deref = sugared '@' "deref"

-- this needs a bit of help to avoid turning ~@foo into (unquote (deref foo))
unquote :: SwjParser PVal
unquote = do omit $ char '~'
             splice <- optionMaybe $ char '@'
             e <- expr
             return $ call (unquoteType splice) [e]
  where unquoteType (Just _) = cljns ++ "unquote-splicing"
        unquoteType Nothing = cljns ++ "unquote"

-- TODO: Should use attr to tag on vals.
--meta :: SwjParser PVal
--meta = do omit $ char '^'
--          m <- expr
--          e <- expr
--          return $ call "with-meta" [e, m]

-- sharpies be here

sharp :: SwjParser PVal
sharp = char '#' >> (sharpQuote <|> set <|> fnLit)

sharpQuote :: SwjParser PVal
sharpQuote = sugared '\'' "var"

fnLit :: SwjParser PVal
fnLit = Fix . PFnLit <$> delimited '(' ')' (many expr)

set :: SwjParser PVal
set = Fix . PSet <$> delimited '{' '}' (many expr)

-- are there more? More non-whitespacey, that is.

expr :: SwjParser PVal
expr = list <|> vec <|> symbol <|> keyword <|> malString <|> hashMap
       <|> quote <|> backquote <|> deref <|> unquote <|> sharp

readAst :: String -> Either SwjError (Maybe PVal)
readAst s = throwLeftMap SyntaxError $
             runParser (whiteSpace >> optionMaybe expr <* eof) False "" s
  where throwLeftMap f (Left x) = throwError (f x)
        throwLeftMap _ (Right x) = return x
