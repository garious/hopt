{-# LANGUAGE Safe #-}

module TsParser where

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , mkInum
  )
import Data.IterIO.Parse
  ( (<|>)
  , (<?>)
  , (<*>)
  , string
  , (<:>)
  , (<$>)
  , (<*)
  , (*>)
  , satisfy
  , eofI
  , eord
  , whileI
  , while1I
  , skipWhileI
  , skipWhile1I
  )
import Data.ListLike
  ( singleton
  )

import Block

import qualified Data.ByteString.Lazy.Char8 as L

parseFlow :: Inum L.ByteString Block IO a
parseFlow = mkInum (whitespace *> parseFlow' <* terminator)

parseFlow' :: Iter L.ByteString IO Block
parseFlow' = varStat
         <|> assignStat
         <|> returnStat
         <|> return []  -- line with a semi-colon

terminator :: Iter L.ByteString IO ()
terminator = skipWhileI isNotTerminator <* satisfy (const True)
         <|> eofI
         <?> "terminator"

isNotTerminator :: (Enum a, Eq a) => a -> Bool
isNotTerminator s = isWhite s && s /= eord '\n' && s /= eord ';'

varStat :: Iter L.ByteString IO Block
varStat = do
    keyword "var"
    nm <- identifier
    as <- (singleton . Assignment nm <$> (token "=" *> expression)) <|> return []
    return $ Declaration TyDynamic nm : as
    <?> "variable declaration"

assignStat :: Iter L.ByteString IO Block
assignStat = singleton <$> (Assignment <$> identifier <*> (token "=" *> expression))
         <?> "assignment"

returnStat :: Iter L.ByteString IO Block
returnStat = singleton . Return <$> (keyword "return" *> expression)
         <?> "return statement"

expression :: Iter L.ByteString IO Expr
expression = ExprVar <$> identifier
         <|> ExprConstant . LitInteger <$> integer
         <?> "expression"

integer :: Iter L.ByteString IO Integer
integer = read . L.unpack <$> while1I isAlphaNumOrUnder

identifier :: Iter L.ByteString IO String
identifier = L.unpack <$> satisfy isLetterOrUnder <:> whileI isAlphaNumOrUnder

isLetterOrUnder :: (Enum a, Ord a) => a -> Bool
isLetterOrUnder s = s >= eord 'a' && s <= eord 'z' || s >= eord 'A' && s <= eord 'Z' || s == eord '_'

isAlphaNumOrUnder :: (Enum a, Ord a) => a -> Bool
isAlphaNumOrUnder s = isLetterOrUnder s || s >= eord '0' && s <= eord '9'

-- Requires some whitespace after keyword
keyword :: String -> Iter L.ByteString IO ()
keyword s = string s >> whitespace1

-- Optional whitespace on either side of a given string
token :: String -> Iter L.ByteString IO ()
token s = whitespace >> string s >> whitespace

whitespace :: Iter L.ByteString IO ()
whitespace = skipWhileI isWhite

whitespace1 :: Iter L.ByteString IO ()
whitespace1 = skipWhile1I isWhite

isWhite :: (Enum a, Eq a) => a -> Bool
isWhite s = s == eord ' ' || s == eord '\n' || s == eord '\t' || s == eord '\r'

