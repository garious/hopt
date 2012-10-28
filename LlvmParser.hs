{-# LANGUAGE Safe #-}

module LlvmParser where

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , mkInum
  )
import Data.IterIO.Parse
  ( (<|>) -- Alternative
  , (<?>) -- Add a name
  , (<*>) -- Apply
  , (<$>) -- Apply pure
  , (<*)  -- Apply but return the left
  , (*>)  -- Apply but return the right
  , string
  , many
  , satisfy
  , eofI
  , eord
  , whileI
  , while1I
  , optionalI
  , skipWhileI
  , skipWhile1I
  , sepBy
  )
import Data.ListLike
  ( singleton
  )

import Block

import qualified Data.ByteString.Lazy.Char8 as L

parseFlow :: Inum L.ByteString Block IO a
parseFlow = mkInum $ basicBlock

basicBlock :: Iter L.ByteString IO Block
basicBlock = whitespace *> (parseFlow' <|> flushStat) <* terminator
         <|> terminator *> return []

parseFlow' :: Iter L.ByteString IO Block
parseFlow' = assignStat
         <|> returnStat

flushStat :: Iter L.ByteString IO Block
flushStat = string ":flush" *> return [Flush]
         <?> "flush signal"

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
returnStat = singleton <$> (Return <$> (keyword "ret" *> llvmType) <*> expression)
         <?> "return statement"

expression :: Iter L.ByteString IO Expr
expression = ExprVar <$> identifier
         <|> ExprConstant . LitInteger <$> integer
         <|> addExpr
         <?> "expression"

addExpr :: Iter L.ByteString IO Expr
addExpr = do
    keyword "add"
    ty <- llvmType
    e1 <- expression
    comma
    e2 <- expression
    return $ ExprAdd ty e1 e2

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

toplevelEntity :: Iter L.ByteString IO ToplevelEntity
toplevelEntity = function
             <|> target
             <?> "top-level entity"

target :: Iter L.ByteString IO ToplevelEntity
target = do
    keyword "target"
    nm <- targetName
    whitespace1
    keyword "="
    s <- stringLiteral
    return $ Target nm s

targetName :: Iter L.ByteString IO String
targetName = fmap L.unpack $ string "datalayout"
                         <|> string "triple"

stringLiteral :: Iter L.ByteString IO String
stringLiteral = do
    _ <- string "\""
    s <- while1I (eord '"' /=)
    _ <- string "\""
    return $ L.unpack s

--define [linkage] [visibility]
--       [cconv] [ret attrs]
--       <ResultType> @<FunctionName> ([argument list])
--       [fn Attrs] [section "name"] [align N]
--       [gc] { ... }
function :: Iter L.ByteString IO ToplevelEntity
function = do
    keyword "define"
    ty <- llvmType
    _  <- string "@"
    nm <- L.unpack <$> (string "@" *> while1I isAlphaNumOrUnder)
    whitespace
    args <- parens (argument `sepBy` comma)
    as   <- many functionAttribute
    bb   <- brackets basicBlock
    return $ Function ty nm args as bb

parens :: Iter L.ByteString IO a -> Iter L.ByteString IO a
parens p = string "(" *> whitespace *> p <* whitespace <* string ")"

brackets :: Iter L.ByteString IO a -> Iter L.ByteString IO a
brackets p = string "{" *> whitespace *> p <* whitespace <* string "}"

argument :: Iter L.ByteString IO String
argument = do
    _ty <- llvmType
    whitespace1
    nm <- identifier
    whitespace
    return nm

comma :: Iter L.ByteString IO ()
comma = whitespace *> string "," *> whitespace

llvmType :: Iter L.ByteString IO String
llvmType = do
    ty  <- intType
    ptr <- fmap L.unpack (string "*") <|> return ""
    whitespace1
    return $ ty ++ ptr

intType :: Iter L.ByteString IO String
intType = fmap L.unpack $
                    string "i1"
                <|> string "i8"
                <|> string "i16"
                <|> string "i32"
                <|> string "i64"
                <|> string "i128"

functionAttribute :: Iter L.ByteString IO String
functionAttribute = fmap L.unpack $
                    string "address_safety"
                <|> string "alignstack"
                <|> string "alwaysinline"
                <|> string "nonlazybind"
                <|> string "inlinehint"
                <|> string "naked"
                <|> string "noimplicitfloat"
                <|> string "noinline"
                <|> string "noredzone"
                <|> string "noreturn"
                <|> string "nounwind"
                <|> string "optsize"
                <|> string "readnone"
                <|> string "readonly"
                <|> string "returns_twice"
                <|> string "ssp"
                <|> string "sspreq"
                <|> string "uwtable"

