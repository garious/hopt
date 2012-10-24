{-# LANGUAGE Safe #-}

module LlvmParser where

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
  , (<$>)
  , (<*)
  , (*>)
  , satisfy
  , eofI
  , eord
  , whileI
  , while1I
  , optionalI
  , skipWhileI
  , skipWhile1I
  )
import Data.ListLike
  ( singleton
  )

import Block

import qualified Data.ByteString.Lazy.Char8 as L

parseFlow :: Inum L.ByteString Block IO a
parseFlow = mkInum $ whitespace *> (parseFlow' <|> flushStat) <* terminator
                 <|> terminator *> return []

parseFlow' :: Iter L.ByteString IO Block
parseFlow' = assignStat
         <|> returnStat

flushStat :: Iter L.ByteString IO Block
flushStat = string ":flush" *> return [Flush]

terminator :: Iter L.ByteString IO ()
terminator = skipWhileI isHorizSpace <* optionalI comment <* satisfy isTerminator
         <|> eofI
         <?> "terminator"

isHorizSpace :: (Enum a, Eq a) => a -> Bool
isHorizSpace s = s == eord ' ' || s == eord '\t'

isTerminator :: (Enum a, Eq a) => a -> Bool
isTerminator s = s == eord '\n'

comment :: Iter L.ByteString IO L.ByteString
comment = satisfy (== eord ';') *> whileI (not . isTerminator)

assignStat :: Iter L.ByteString IO Block
assignStat = singleton <$> (Assignment <$> identifier <*> (token "=" *> expression))
         <?> "assignment"

returnStat :: Iter L.ByteString IO Block
returnStat = singleton . Return <$> (keyword "ret" *> expression)
         <?> "return statement"

expression :: Iter L.ByteString IO Expr
expression = ExprVar <$> identifier
         <|> ExprConstant . LitInteger <$> integer
         <?> "expression"

integer :: Iter L.ByteString IO Integer
integer = read . L.unpack <$> while1I isNum

identifier :: Iter L.ByteString IO String
identifier = L.unpack <$> (string "%" *> while1I isAlphaNumOrUnder)

isLetterOrUnder :: (Enum a, Ord a) => a -> Bool
isLetterOrUnder s = s >= eord 'a' && s <= eord 'z' || s >= eord 'A' && s <= eord 'Z' || s == eord '_'

isAlphaNumOrUnder :: (Enum a, Ord a) => a -> Bool
isAlphaNumOrUnder s = isLetterOrUnder s || isNum s

isNum :: (Enum a, Ord a) => a -> Bool
isNum s = s >= eord '0' && s <= eord '9'

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

