{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A parser for a very small subset of LLVM.  What is unique about these
--   parser combinators is that they parse whitespace preceding each token
--   instead of after them.  This means the parser will return as soon as
--   possible, and not hang waiting for whitespace.

module LlvmParser where

import Data.Attoparsec.ByteString
  ( (<?>) -- Add a name
  , string
  , satisfy
  , endOfInput
  , takeWhile
  , takeWhile1
  , skipWhile
  , sepBy
  , sepBy1
  , inClass
  , Parser
  )
import Data.Attoparsec.ByteString.Char8
  ( skipSpace
  , space
  , decimal
  , isDigit_w8
  )
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
import Data.Char
  ( ord
  )
import Data.Word
  ( Word8
  )
import Prelude hiding (takeWhile)

import LlvmData

import qualified Data.ByteString.Char8 as BC

-- | Parses a basic block
basicBlock :: Parser Block
basicBlock = concat <$> many basicBlock'

-- | Parses a statement followed by a terminator or a terminator
basicBlock' :: Parser Block
basicBlock' = (: []) <$> (skipSpace *> (stat <|> flushStat) <* terminator)
         <|> terminator *> return []

-- | Parses a statement
stat :: Parser Statement
stat       = assignStat
         <|> returnStat
         <|> branchStat
         <|> branchCondStat
         <|> labelStat

-- | Parses an unconditional branch statement
branchStat :: Parser Statement
branchStat = do
    keyword "br"
    keyword "label"
    Branch <$> identifier

-- br i1 %cond, label %IfEqual, label %IfUnequal
-- | Parses a conditional branch statement
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

-- | Parses a Boolean literal
boolLit :: Parser Literal
boolLit = (string "true"  >> return (LitBool True))
      <|> (string "false" >> return (LitBool False))


-- | Parses a special "flush" statement, used to flush the parser pipeline
flushStat :: Parser Statement
flushStat = string ":flush" *> return Flush
         <?> "flush signal"

-- | Parses a statement terminator
terminator :: Parser ()
terminator = skipWhile isHorizSpace <* optional comment <* satisfy isTerminator
         <|> endOfInput
         <?> "terminator"

-- | Returns true if the input character is a tab or space
isHorizSpace :: (Enum a, Eq a) => a -> Bool
isHorizSpace s = s == eord ' ' || s == eord '\t'

-- | Same as ord, but returns any Enum instead of an Int
eord :: Enum a => Char -> a
eord = toEnum . ord

-- | Returns true if the input character is a terminator
isTerminator :: (Enum a, Eq a) => a -> Bool
isTerminator s = s == eord '\n'

-- | Parses a comment
comment :: Parser BC.ByteString
comment = satisfy (== eord ';') *> takeWhile (not . isTerminator)

-- | Parses an assignment statement
assignStat :: Parser Statement
assignStat = Assignment <$> identifier <*> (token "=" *> expression)
         <?> "assignment"

-- | Parses a return statement
returnStat :: Parser Statement
returnStat = Return <$> (keyword "ret" *> llvmType) <*> expression
         <?> "return statement"

-- | Parses an expression
expression :: Parser Expr
expression = ExprVar <$> identifier
         <|> ExprConstant . LitInteger <$> decimal
         <|> addExpr
         <|> phiExpr
         <?> "expression"

-- | Parses an add expression
addExpr :: Parser Expr
addExpr = do
    keyword "add"
    ty <- llvmType
    e1 <- expression
    comma
    e2 <- expression
    return $ ExprAdd ty e1 e2

-- | Parses a phi expression
phiExpr :: Parser Expr
phiExpr = do
    keyword "phi"
    ty <- llvmType
    es <- brackets phiField `sepBy1` comma
    return $ ExprPhi ty es

-- | Parses a phi field
phiField :: Parser (Expr, String)
phiField = (,) <$> (expression <* comma) <*> identifier

-- | Parses a global variable
globalVar :: Parser String
globalVar = BC.unpack <$> (string "@" *> takeWhile1 isAlphaNumOrUnder)

-- | Parses an identifier
identifier :: Parser String
identifier = BC.unpack <$> (string "%" *> takeWhile1 isAlphaNumOrUnder)

-- | Parses a label
labelStat :: Parser Statement
labelStat = (Label . BC.unpack) <$> (takeWhile1 isAlphaNumOrUnder <* string ":")

-- | Parses an alphanumeric character or an underscore
isAlphaNumOrUnder :: Word8 -> Bool
isAlphaNumOrUnder s = inClass "a-zA-Z" s || isDigit_w8 s || eord '_' == s

-- | Requires some whitespace after keyword
keyword :: BC.ByteString -> Parser ()
keyword s = string s >> skipSpace1

-- | Optional whitespace on either side of a given string
token :: BC.ByteString -> Parser ()
token s = skipSpace >> string s >> skipSpace

-- | Parse one or more whitespace characters
skipSpace1 :: Parser ()
skipSpace1 = space >> skipSpace

-- | Parses a list of top-level entities
toplevelEntities :: Parser Module
toplevelEntities = (do
    x <- skipSpace *> toplevelEntity
    return [x])
 <|> (terminator *> return [])

-- | Parses a top-level entity
toplevelEntity :: Parser ToplevelEntity
toplevelEntity = function
             <|> target
             <?> "top-level entity"

-- | Parses a target definition
target :: Parser ToplevelEntity
target = do
    keyword "target"
    nm <- targetName
    skipSpace
    keyword "="
    s <- stringLiteral
    return $ Target nm s

-- | Parses a target name
targetName :: Parser String
targetName = BC.unpack <$> (string "datalayout" <|> string "triple")

-- | Parses a string literal
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

-- | Parses a function definition
function :: Parser ToplevelEntity
function = do
    keyword "define"
    ty <- llvmType
    nm <- globalVar
    skipSpace
    args <- parens (parameter `sepBy` comma)
    skipSpace
    as   <- many functionAttribute
    bb   <- braces basicBlock
    return $ Function ty nm args as bb

-- | Parses 'p' within parens
parens :: Parser a -> Parser a
parens p = string "(" *> skipSpace *> p <* skipSpace <* string ")"

-- | Parses 'p' within braces
braces :: Parser a -> Parser a
braces p = string "{" *> skipSpace *> p <* skipSpace <* string "}"

-- | Parses 'p' within brackets
brackets :: Parser a -> Parser a
brackets p = string "[" *> skipSpace *> p <* skipSpace <* string "]"

-- | Parses a function parameter
parameter :: Parser Parameter
parameter = do
    skipSpace
    Parameter <$> llvmType <*> identifier

-- | Parses a comma
comma :: Parser ()
comma = skipSpace *> string "," *> skipSpace

-- | Parses an LLVM type
llvmType :: Parser String
llvmType = do
    ty  <- intType
    ptr <- (BC.unpack <$> string "*") <|> return ""
    skipSpace1
    return $ ty ++ ptr

-- | Parses an integer type
intType :: Parser String
intType = BC.unpack <$> (
                    string "i1"
                <|> string "i8"
                <|> string "i16"
                <|> string "i32"
                <|> string "i64"
                <|> string "i128"
                )

-- | Parses a function attribute
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

