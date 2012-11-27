{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module LlvmParser where

-- From IterIO package
import Data.IterIO
  ( Inum
  , mkInum
  )
import Data.Attoparsec.ByteString.Lazy
  ( (<?>) -- Add a name
  , string
  , satisfy
  , endOfInput
  , takeWhile1
  , skipWhile
  , sepBy
  , sepBy1
  , Parser
  )
import qualified Data.Attoparsec.ByteString.Lazy as A
import Control.Applicative
  ( (<*>) -- Apply
  , (<$>) -- Apply pure
  , (<*)  -- Apply but return the left
  , (*>)  -- Apply but return the right
  , (<|>)
  , optional
  , many
  , Alternative
  )
import Data.IterIO.Atto
  ( atto
  )
import Data.Char
  ( ord
  )

import Block

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BC


parseFlow :: Inum L.ByteString Module IO a
parseFlow = mkInum $ atto toplevelEntities

basicBlock :: Parser Block
basicBlock = concat <$> many basicBlock'

basicBlock' :: Parser Block
basicBlock' = (: []) <$> (whitespace *> (stat <|> flushStat) <* terminator)
         <|> terminator *> return []

stat :: Parser Statement
stat       = assignStat
         <|> returnStat
         <|> branchStat
         <|> branchCondStat
         <|> labelStat

branchStat :: Parser Statement
branchStat = do
    keyword "br"
    keyword "label"
    Branch <$> identifier

-- br i1 %cond, label %IfEqual, label %IfUnequal
branchCondStat :: Parser Statement
branchCondStat = do
    keyword "br"
    keyword "i1"
    expr <- (ExprVar <$> identifier) <|> (ExprConstant <$> boolLit)
    comma
    keyword "label"
    left <- identifier
    comma
    keyword "label"
    right <- identifier
    return $ BranchCond expr left right

boolLit :: Parser Literal
boolLit = (string "true"  >> return (LitBool True))
      <|> (string "false" >> return (LitBool False))


flushStat :: Parser Statement
flushStat = string ":flush" *> return Flush
         <?> "flush signal"

terminator :: Parser ()
terminator = skipWhile isHorizSpace <* optional comment <* satisfy isTerminator
         <|> endOfInput
         <?> "terminator"

isHorizSpace :: (Enum a, Eq a) => a -> Bool
isHorizSpace s = s == eord ' ' || s == eord '\t'

eord :: Enum a => Char -> a
eord = toEnum . ord

isTerminator :: (Enum a, Eq a) => a -> Bool
isTerminator s = s == eord '\n'

comment :: Parser BC.ByteString
comment = satisfy (== eord ';') *> A.takeWhile (not . isTerminator)

assignStat :: Parser Statement
assignStat = Assignment <$> identifier <*> (token "=" *> expression)
         <?> "assignment"

returnStat :: Parser Statement
returnStat = Return <$> (keyword "ret" *> llvmType) <*> expression
         <?> "return statement"

expression :: Parser Expr
expression = ExprVar <$> identifier
         <|> ExprConstant . LitInteger <$> integer
         <|> addExpr
         <|> phiExpr
         <?> "expression"

addExpr :: Parser Expr
addExpr = do
    keyword "add"
    ty <- llvmType
    e1 <- expression
    comma
    e2 <- expression
    return $ ExprAdd ty e1 e2

phiExpr :: Parser Expr
phiExpr = do
    keyword "phi"
    ty <- llvmType
    es <- brackets phiSource `sepBy1` comma
    return $ ExprPhi ty es

phiSource :: Parser (Expr, String)
phiSource = (,) <$> (expression <* comma) <*> identifier

integer :: Parser Integer
integer = read . BC.unpack <$> takeWhile1 isNum

globalVar :: Parser String
globalVar = BC.unpack <$> (string "@" *> takeWhile1 isAlphaNumOrUnder)

identifier :: Parser String
identifier = BC.unpack <$> (string "%" *> takeWhile1 isAlphaNumOrUnder)

labelStat :: Parser Statement
labelStat = (Label . BC.unpack) <$> (takeWhile1 isAlphaNumOrUnder <* string ":")

isLetterOrUnder :: (Enum a, Ord a) => a -> Bool
isLetterOrUnder s = s >= eord 'a' && s <= eord 'z' || s >= eord 'A' && s <= eord 'Z' || s == eord '_'

isAlphaNumOrUnder :: (Enum a, Ord a) => a -> Bool
isAlphaNumOrUnder s = isLetterOrUnder s || isNum s

isNum :: (Enum a, Ord a) => a -> Bool
isNum s = s >= eord '0' && s <= eord '9'

-- Requires some whitespace after keyword
keyword :: BC.ByteString -> Parser ()
keyword s = string s >> whitespace1

-- Optional whitespace on either side of a given string
token :: BC.ByteString -> Parser ()
token s = whitespace >> string s >> whitespace

whitespace :: Parser ()
whitespace = skipWhile isWhite

whitespace1 :: Parser ()
whitespace1 = takeWhile1 isWhite *> return ()

isWhite :: (Enum a, Eq a) => a -> Bool
isWhite s = s == eord ' ' || s == eord '\n' || s == eord '\t' || s == eord '\r'

toplevelEntities :: Parser Module
toplevelEntities = (do
    x <- whitespace *> toplevelEntity
    return [x])
 <|> (terminator *> return [])

toplevelEntity :: Parser ToplevelEntity
toplevelEntity = function
             <|> target
             <?> "top-level entity"

target :: Parser ToplevelEntity
target = do
    keyword "target"
    nm <- targetName
    whitespace
    keyword "="
    s <- stringLiteral
    return $ Target nm s

targetName :: Parser String
targetName = BC.unpack <$> (string "datalayout" <|> string "triple")

stringLiteral :: Parser String
stringLiteral = do
    _ <- string "\""
    s <- takeWhile1 (eord '"' /=)
    _ <- string "\""
    return $ BC.unpack s

--define [linkage] [visibility]
--       [cconv] [ret attrs]
--       <ResultType> @<FunctionName> ([argument list])
--       [fn Attrs] [section "name"] [align N]
--       [gc] { ... }
function :: Parser ToplevelEntity
function = do
    keyword "define"
    ty <- llvmType
    nm <- globalVar
    whitespace
    args <- parens (argument `sepBy` comma)
    whitespace
    as   <- many functionAttribute
    bb   <- braces basicBlock
    return $ Function ty nm args as bb

parens :: Parser a -> Parser a
parens p = string "(" *> whitespace *> p <* whitespace <* string ")"

braces :: Parser a -> Parser a
braces p = string "{" *> whitespace *> p <* whitespace <* string "}"

brackets :: Parser a -> Parser a
brackets p = string "[" *> whitespace *> p <* whitespace <* string "]"

argument :: Parser String
argument = do
    whitespace
    _ty <- llvmType
    identifier

comma :: Parser ()
comma = whitespace *> string "," *> whitespace

llvmType :: Parser String
llvmType = do
    ty  <- intType
    ptr <- (BC.unpack <$> string "*") <|> return ""
    whitespace1
    return $ ty ++ ptr

intType :: Parser String
intType = BC.unpack <$> (
                    string "i1"
                <|> string "i8"
                <|> string "i16"
                <|> string "i32"
                <|> string "i64"
                <|> string "i128"
                )

functionAttribute :: Parser String
functionAttribute = BC.unpack <$> (
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
                )

