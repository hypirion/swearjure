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

data ParseVal p = PSym String -- qualified syms. Gur
                | PString String
                | PKw String
                | PQualKw String
                | PChar Char
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
omit :: Parser p -> Parser ()
omit p = p >> return ()

-- | Semi matches a semicolon and returns
semi :: Parser Char
semi = char ';'

commentLine :: Parser String
commentLine = skipMany1 semi >> manyTill anyChar (omit newline <|> eof)

-- | Filters out any kind of whitespace.
whiteSpace :: Parser ()
whiteSpace = skipMany (omit (char ',') <|> omit space <|> omit commentLine
                       <|> try sharpWhites)

sharpWhites :: Parser ()
sharpWhites = char '#' >> (sharp_ <|> sharpBang)

sharp_ :: Parser ()
sharp_ = lexeme (char '_') >> omit expr

sharpBang :: Parser ()
sharpBang = char '!' >> manyTill anyChar (omit newline <|> eof) >> return ()

lexeme :: Parser p -> Parser p
lexeme p = do x <- p
              whiteSpace
              return x

-- fixme: These are.. nasty to do properly.
startSymChar :: Parser Char
startSymChar = oneOf "+-*/!=<>?_&$%"

symChar :: Parser Char
symChar = startSymChar <|> oneOf "#'"

symString :: Parser String
symString = do x <- startSymChar
               xs <- many symChar
               return $ x : xs

symbol :: Parser PVal
symbol = (Fix . PSym)  <$> lexeme symString

-- TODO: qualified kws
keyword :: Parser PVal
keyword = Fix . PKw <$> lexeme (char ':' >> symString)

malString :: Parser PVal
malString = Fix . PString <$> (lexeme $ between (char '"') (char '"')
                               (many stringChar))
  where stringChar = stringLetter <|> stringEscape
                     <?> "string character (non-alphanumeric)"
        stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (not $ isAlphaNum c))
        -- Todo: Fix this properly?
        stringEscape = char '\\' >> oneOf "\\\""

delimited :: Char -> Char -> Parser c -> Parser c
delimited start stop = between (lexeme $ char start) (lexeme $ char stop)

list :: Parser PVal
list = Fix . PList <$> delimited '(' ')' (many expr)

vec :: Parser PVal
vec = Fix . PVec <$> delimited '[' ']' (many expr)

hashMap :: Parser PVal
hashMap = Fix . PHM <$> delimited '{' '}' (many pair)
  where pair = (,) <$> expr <*> expr

call :: String -> [PVal] -> PVal
call s es = Fix . PList $ (Fix . PSym) s : es

sugared :: Char -> String -> Parser PVal
sugared c s = do (omit . lexeme . char) c
                 e <- expr
                 return $ call (cljns ++ s) [e]

quote :: Parser PVal
quote = sugared '\'' "quote"

-- Alright, this is obv. wrong. Need to look into the reader properly, because
-- it does some fancy read time replacement.
backquote :: Parser PVal
backquote = sugared '`' "quote"

deref :: Parser PVal
deref = sugared '@' "deref"

-- this needs a bit of help to avoid turning ~@foo into (unquote (deref foo))
unquote :: Parser PVal
unquote = do omit $ char '~'
             splice <- optionMaybe $ char '@'
             e <- expr
             return $ call (unquoteType splice) [e]
  where unquoteType (Just _) = cljns ++ "unquote-splicing"
        unquoteType Nothing = cljns ++ "unquote"

-- TODO: Should use attr to tag on vals.
--meta :: Parser PVal
--meta = do omit $ char '^'
--          m <- expr
--          e <- expr
--          return $ call "with-meta" [e, m]

-- sharpies be here

sharp :: Parser PVal
sharp = char '#' >> (sharpQuote <|> set)

sharpQuote :: Parser PVal
sharpQuote = sugared '\'' "var"

set :: Parser PVal
set = Fix . PSet <$> delimited '{' '}' (many expr)

-- are there more? More non-whitespacey, that is.

expr :: Parser PVal
expr = list <|> vec <|> symbol <|> keyword <|> malString <|> hashMap
       <|> quote <|> backquote <|> deref <|> unquote <|> sharp

readAst :: String -> Either SwjError (Maybe PVal)
readAst s = throwLeftMap SyntaxError $
             parse (whiteSpace >> optionMaybe expr <* eof) "" s
  where throwLeftMap f (Left x) = throwError (f x)
        throwLeftMap _ (Right x) = return x
