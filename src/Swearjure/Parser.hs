{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Swearjure.Parser
       ( PValF(..)
       , PVal
       , readAsts
       , readStatic
       , ParseResult(..)
       , feedCont
       ) where

import           Control.Applicative hiding (many)
import           Control.Monad.Reader
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as L
import           Data.Char (isAlphaNum)
import           Data.Foldable (Foldable)
import           Data.Generics.Fixplate (Mu(..), ShowF(..), EqF(..), OrdF(..))
import           Data.Sequence
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as B
import           Data.Traversable (Traversable)
import           Swearjure.Errors

data PValF p = PSym String
             | PString String
             | PKw String
             | PQualKw String
             | PChar Char
             | PFnLit [p]
             | PList [p]
             | PVec [p]
             | PSet [p]
             | PHM [(p, p)]
             | PSyntaxQuote p
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type PVal = Mu PValF

instance EqF PValF where equalF = (==)
instance OrdF PValF where compareF = compare
instance ShowF PValF where showsPrecF = showsPrec

cljns :: String
cljns = "clojure.core/"

-- | Discards the result of the parser
omit :: Parser p -> Parser ()
omit p = p >> return ()

-- | Semi matches a semicolon and returns it
semi :: Parser Char
semi = char ';'

commentLine :: Parser String
commentLine = skipMany1 semi >> manyTill anyChar (endOfLine <|> endOfInput)

-- | Filters out any kind of whitespace.
whiteSpace :: Parser ()
whiteSpace = skipMany (omit (char ',') <|> omit space <|> omit commentLine
                       <|> try sharpWhites)

sharpWhites :: Parser ()
sharpWhites = char '#' >> (sharp_ <|> sharpBang)

sharp_ :: Parser ()
sharp_ = char '_' >> omit expr

sharpBang :: Parser ()
sharpBang = char '!' >> manyTill anyChar (endOfLine <|> endOfInput) >> return ()

lexeme :: Parser p -> Parser p
lexeme p = whiteSpace >> p

anyOf :: String -> Parser Char
anyOf = satisfy . inClass

-- fixme: These are.. nasty to do properly.
startSymChar :: Parser Char
startSymChar = anyOf "-+*/!=<>?_&$%"

symChar :: Parser Char
symChar = startSymChar <|> anyOf "#'"

-- TODO: If sym only contains single /, then / can't be the last value UNLESS
-- the symbol is "/".
symString :: Parser String
symString = do x <- startSymChar
               xs <- many' symChar
               return $ x : xs

symbol :: Parser PVal
symbol = (Fix . PSym) <$> symString

keyword :: Parser PVal
keyword = char ':' >> (nonQual <|> qual <|> alphaNums)
  where nonQual = Fix . PKw <$> symString
        qual = Fix . PQualKw <$> (char ':' >> symString)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do { omit open; v <- p; omit close; return v }

sjwString :: Parser PVal
sjwString = Fix . PString <$> (between (char '"') (char '"')
                               (many' stringChar))
  where stringChar = stringLetter <|> stringEscape
                     <?> "string character (non-alphanumeric)"
        stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && not (isAlphaNum c))
        -- Todo: Fix this properly?
        stringEscape = char '\\' >> anyOf "\\\""

delimited :: Char -> Char -> Parser c -> Parser c
delimited start stop = between (char start) (lexeme $ char stop)

list :: Parser PVal
list = Fix . PList <$> delimited '(' ')' (many' expr)

vec :: Parser PVal
vec = Fix . PVec <$> delimited '[' ']' (many' expr)

hashMap :: Parser PVal
hashMap = Fix . PHM <$> delimited '{' '}' (many' pair)
  where pair = (,) <$> expr <*> expr

call :: String -> [PVal] -> PVal
call s es = Fix . PList $ (Fix . PSym) s : es

sugared :: Char -> String -> Parser PVal
sugared c s = do omit (char c)
                 e <- expr
                 return $ call (cljns ++ s) [e]

quote :: Parser PVal
quote = do omit (char '\'')
           e <- expr
           return $ call "quote" [e]

syntaxQuote :: Parser PVal
syntaxQuote = char '`' >> (Fix . PSyntaxQuote <$> expr)

deref :: Parser PVal
deref = sugared '@' "deref"

optionMaybe :: Parser p -> Parser (Maybe p)
optionMaybe p = option Nothing (liftM Just p)

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
sharp = char '#' >> (sharpQuote <|> set <|> fnLit False <|> unreadable)

sharpQuote :: Parser PVal
sharpQuote =  do omit (char '\'')
                 e <- expr
                 return $ call "var" [e]

-- must propagate the Bool over the affected results
fnLit :: Bool -> Parser PVal
fnLit True = fail "Nested #() are not allowed"
fnLit False = do omit $ char '('
                 exps <- many' expr
                 omit $ char ')'
                 return $ Fix $ PFnLit exps

set :: Parser PVal
set = Fix . PSet <$> delimited '{' '}' (many' expr)

unreadable :: Parser a
unreadable = char '<' >> fail "Unreadable form"

-- TODO: #=
-- and #?

character :: Parser PVal
character = char '\\' >> (Fix . PChar <$> satisfy (not . isAlphaNum))

alphaNums :: Parser a
alphaNums = satisfy isAlphaNum >> fail "Alphanumeric characters are not allowed"

expr :: Parser PVal
expr = do whiteSpace
          list <|> vec <|> symbol <|> keyword <|> sjwString <|> hashMap
            <|> quote <|> syntaxQuote <|> deref <|> unquote <|> sharp <|>
            character <|> alphaNums

justWS :: String -> Bool
justWS s
  = case parse whiteSpace (T.pack s) of
     (Fail _ _ _) -> False
     (Partial _) -> True
     (Done txt ()) -> T.null txt

type ResultSeq = (Seq (Either SwjError PVal))
type ParseFn = T.Text -> Result PVal

-- parsing != "reading". A PVal is expanded because of reader conditionals
data ParseResult = Results ResultSeq String
                 | Continuation ResultSeq String ParseFn

-- feeds Continuations more data. The continuation must be unwrapped
feedCont :: String -> ResultSeq -> String -> ParseFn -> ParseResult
feedCont s xs accum cont
  = merge $ readRec (cont $ T.pack $ s ++ "\n")
    where merge (vals, Nothing) = Results (xs >< vals) (accum ++ ('\n' : s))
          merge (vals, Just newCont) = Continuation (xs >< vals)
                                       (accum ++ ('\n' : s)) newCont

readRec :: Result PVal -> (ResultSeq, Maybe ParseFn)
readRec (Fail _ _ err) = (S.singleton $ Left $ SyntaxError err, Nothing)
readRec (Partial cont) = (S.empty, Just cont)
readRec (Done txt r) | justWS (T.unpack txt) = (S.singleton $ Right r, Nothing)
readRec (Done txt r) = recur deeper
  where deeper = readRec $ parse (whiteSpace >> expr) txt
        recur (x, y) = (Right r <| x, y)

readAsts :: String -> ParseResult
readAsts s | justWS s = Results S.empty s
readAsts s = foo $ readRec $ parse (whiteSpace >> expr) (T.pack $ s ++ "\n")
  where foo (vals, Nothing) = Results vals s
        foo (vals, Just cont) = Continuation vals s cont

-- difference between readStatic and readAsts is that readStatic is incremental,
-- and won't give back continuations. Consequently it just gobbles until it's
-- done. Passes back a list to avoid the strictness we get with Sequences.
readStatic :: B.Text -> [Either SwjError PVal]
readStatic s | lazyWS s = []
readStatic s = go $ L.parse (whiteSpace >> expr) s
  where go (L.Fail _ _ err) = [Left $ SyntaxError err]
        go (L.Done txt r) = Right r : readStatic txt

lazyWS :: B.Text -> Bool
lazyWS s = case L.parse whiteSpace s of
            (L.Fail _ _ _) -> False
            (L.Done txt ()) -> B.null txt
