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
  , sepBy1
  )
import Control.Applicative
  ( (<*>) -- Apply
  , (<$>) -- Apply pure
  , (<*)  -- Apply but return the left
  , (*>)  -- Apply but return the right
  )
import Data.ListLike
  ( singleton
  )

import Block

import qualified Data.ByteString.Lazy.Char8 as L

parseFlow :: Inum L.ByteString Module IO a
parseFlow = mkInum toplevelEntities

basicBlock :: Iter L.ByteString IO Block
basicBlock = concat <$> many basicBlock'

basicBlock' :: Iter L.ByteString IO Block
basicBlock' = singleton <$> (whitespace *> (stat <|> flushStat) <* terminator)
         <|> terminator *> return []

stat :: Iter L.ByteString IO Statement
stat       = assignStat
         <|> returnStat
         <|> branchStat
         <|> branchCondStat
         <|> labelStat

branchStat :: Iter L.ByteString IO Statement
branchStat = do
    keyword "br"
    keyword "label"
    Branch <$> identifier

-- br i1 %cond, label %IfEqual, label %IfUnequal
branchCondStat :: Iter L.ByteString IO Statement
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

boolLit :: Iter L.ByteString IO Literal
boolLit = (string "true"  >> return (LitBool True))
      <|> (string "false" >> return (LitBool False))


flushStat :: Iter L.ByteString IO Statement
flushStat = string ":flush" *> return Flush
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

assignStat :: Iter L.ByteString IO Statement
assignStat = Assignment <$> identifier <*> (token "=" *> expression)
         <?> "assignment"

returnStat :: Iter L.ByteString IO Statement
returnStat = Return <$> (keyword "ret" *> llvmType) <*> expression
         <?> "return statement"

expression :: Iter L.ByteString IO Expr
expression = ExprVar <$> identifier
         <|> ExprConstant . LitInteger <$> integer
         <|> addExpr
         <|> phiExpr
         <?> "expression"

addExpr :: Iter L.ByteString IO Expr
addExpr = do
    keyword "add"
    ty <- llvmType
    e1 <- expression
    comma
    e2 <- expression
    return $ ExprAdd ty e1 e2

phiExpr :: Iter L.ByteString IO Expr
phiExpr = do
    keyword "phi"
    ty <- llvmType
    es <- brackets phiSource `sepBy1` comma
    return $ ExprPhi ty es

phiSource :: Iter L.ByteString IO (Expr, String)
phiSource = (,) <$> (expression <* comma) <*> identifier

integer :: Iter L.ByteString IO Integer
integer = read . L.unpack <$> while1I isNum

globalVar :: Iter L.ByteString IO String
globalVar = L.unpack <$> (string "@" *> while1I isAlphaNumOrUnder)

identifier :: Iter L.ByteString IO String
identifier = L.unpack <$> (string "%" *> while1I isAlphaNumOrUnder)

labelStat :: Iter L.ByteString IO Statement
labelStat = (Label . L.unpack) <$> (while1I isAlphaNumOrUnder <* string ":")

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

toplevelEntities :: Iter L.ByteString IO Module
toplevelEntities = (do
    x <- whitespace *> toplevelEntity
    return [x])
 <|> (terminator *> return [])

toplevelEntity :: Iter L.ByteString IO ToplevelEntity
toplevelEntity = function
             <|> target
             <?> "top-level entity"

target :: Iter L.ByteString IO ToplevelEntity
target = do
    keyword "target"
    nm <- targetName
    whitespace
    keyword "="
    s <- stringLiteral
    return $ Target nm s

targetName :: Iter L.ByteString IO String
targetName = L.unpack <$> (string "datalayout" <|> string "triple")

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
    nm <- globalVar
    whitespace
    args <- parens (argument `sepBy` comma)
    whitespace
    as   <- many functionAttribute
    bb   <- braces basicBlock
    return $ Function ty nm args as bb

parens :: Iter L.ByteString IO a -> Iter L.ByteString IO a
parens p = string "(" *> whitespace *> p <* whitespace <* string ")"

braces :: Iter L.ByteString IO a -> Iter L.ByteString IO a
braces p = string "{" *> whitespace *> p <* whitespace <* string "}"

brackets :: Iter L.ByteString IO a -> Iter L.ByteString IO a
brackets p = string "[" *> whitespace *> p <* whitespace <* string "]"

argument :: Iter L.ByteString IO String
argument = do
    whitespace
    _ty <- llvmType
    identifier

comma :: Iter L.ByteString IO ()
comma = whitespace *> string "," *> whitespace

llvmType :: Iter L.ByteString IO String
llvmType = do
    ty  <- intType
    ptr <- (L.unpack <$> string "*") <|> return ""
    whitespace1
    return $ ty ++ ptr

intType :: Iter L.ByteString IO String
intType = L.unpack <$> (
                    string "i1"
                <|> string "i8"
                <|> string "i16"
                <|> string "i32"
                <|> string "i64"
                <|> string "i128"
                )

functionAttribute :: Iter L.ByteString IO String
functionAttribute = L.unpack <$> (
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

