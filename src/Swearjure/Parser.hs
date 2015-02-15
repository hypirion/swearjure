{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Swearjure.Parser
       ( PValF(..)
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

data PValF p = PSym String
             | PString String
             | PKw String
             | PQualKw String
             | PChar Char
             | PFnLit [p]
             | PList [p]
             | PVec [p]
             | PSet [p] -- Sharpie!
             | PHM [(p, p)]
             | PSyntaxQuote p
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type PVal = Mu PValF

instance ShowF PValF where showsPrecF = showsPrec

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

keyword :: SwjParser PVal
keyword = lexeme (char ':' >> (nonQual <|> qual <|> alphaNums))
  where nonQual = Fix . PKw <$> symString
        qual = Fix . PQualKw <$> (char ':' >> symString)

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
quote = do (omit . lexeme . char) '\''
           e <- expr
           return $ call "quote" [e]

-- Alright, this is obv. wrong. Need to look into the reader properly, because
-- it does some fancy read time replacement.
syntaxQuote :: SwjParser PVal
syntaxQuote = char '`' >> (Fix . PSyntaxQuote <$> expr)

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
sharp = char '#' >> (sharpQuote <|> set <|> fnLit <|> unreadable)

sharpQuote :: SwjParser PVal
sharpQuote = sugared '\'' "var"

fnLit :: SwjParser PVal
fnLit = do omit $ char '('
           insideFnLit <- getState
           when insideFnLit $ fail "Nested #() are not allowed"
           setState True
           exps <- many expr
           omit $ char ')'
           setState False
           whiteSpace
           return $ Fix $ PFnLit exps

set :: SwjParser PVal
set = Fix . PSet <$> delimited '{' '}' (many expr)

unreadable :: SwjParser a
unreadable = char '<' >> fail "Unreadable form"

-- TODO: #=

character :: SwjParser PVal
character = char '\\' >> (Fix . PChar <$> satisfy (not . isAlphaNum))

alphaNums :: SwjParser a
alphaNums = satisfy isAlphaNum >> fail "Alphanumeric characters are not allowed"

expr :: SwjParser PVal
expr = list <|> vec <|> symbol <|> keyword <|> malString <|> hashMap
       <|> quote <|> syntaxQuote <|> deref <|> unquote <|> sharp <|> character
       <|> alphaNums

readAst :: (MonadError SwjError m) => String -> m (Maybe PVal)
readAst s = throwLeftMap SyntaxError $
             runParser (whiteSpace >> optionMaybe expr <* eof) False "" s
  where throwLeftMap f (Left x) = throwError (f x)
        throwLeftMap _ (Right x) = return x
