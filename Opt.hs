{-# LANGUAGE Safe #-}

module Opt where

import ArgParser
  ( Cfg(Cfg)
  , optPasses
  , inFile
  , outFile
  , optPasses
  , _isText
  )
import System.IO
  ( openFile
  , IOMode(WriteMode)
  , stdout
  )

-- From fclabels package
import qualified Data.Label.Pure as Lens

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , (|$)
  , (|.)
  , (.|)
  , enumFile'
  , enumStdin
  , mkInum
  , mkInumAutoM
  , ifeed
  , ipopresid
  , ungetI
  , handleI
  , inumNop
  , dataI
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
import Data.IterIO.Trans
  ( runStateTLI
  , liftI
  )
import Data.ListLike
  ( ListLike
  , singleton
  )

-- From mtl package
import Control.Monad.State
  ( StateT
  , modify
  , get
  )
--import Control.Monad.Trans
--  ( liftIO
--  )

import qualified Data.ByteString.Lazy.Char8 as L

type OptPass a = Inum Block Block IO a

compile :: Cfg -> IO ()
compile Cfg{_isText=False} = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
compile cfg  = do
    hdl <- outPath == "-" ? (return stdout, openFile outPath WriteMode)
    input |$ parseFlow .| optimize (Lens.get optPasses cfg) .| printFlow .| handleI hdl
  where
    input = inPath == "-" ? (enumStdin, enumFile' inPath)
    inPath  = Lens.get inFile cfg
    outPath = Lens.get outFile cfg

type Block = [Statement]

data Statement = Declaration Type String
               | Assignment String Expr
               | Return Expr
  deriving (Show, Eq)

data Type = TyInteger
          | TyString
          | TyDynamic
  deriving (Show, Eq)

data Expr   = ExprConstant Literal
            | ExprVar String
            | ExprNil
  deriving (Show, Eq)

data Literal = LitString String
             | LitInteger Integer
  deriving (Show, Eq)

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

optimize :: [String] -> Inum Block Block IO a
optimize []     = inumNop
optimize (x:xs) = maybe (error x) id (lookup x optPassMap) |. optimize xs

printFlow :: Inum Block L.ByteString IO a
printFlow = mkInum prettyFlow

prettyFlow :: Iter Block IO L.ByteString
prettyFlow  = L.pack . (++"\n") . show <$> dataI

-- | An alternative to the if-then-else syntax
(?) :: Bool -> (a, a) -> a
b ? (t, e) = if b then t else e
infixl 1 ?

optPassNames :: [String]
optPassNames = map fst optPassMap

-- | A map from command-line names to the function that implements an
--   optimization pass
optPassMap :: [(String, OptPass a)]
optPassMap = [
    ("constprop", constProp)
  ]

-- \ The Constant Propogation pass
constProp :: Inum Block Block IO a
constProp = mkInumAutoM (loop [])
  where 
    loop st = do
      (t', st') <- liftI (runStateTLI constPropChunk st)
      done <- ifeed t'
      if not done then
        loop st'
      else
        ipopresid >>= ungetI

type StateIO a = StateT a IO
type ConstMap = [(String, Literal)]

constPropChunk :: Iter Block (StateIO ConstMap) Block
constPropChunk = concat <$> (dataI >>= mapM constPropStat)

constPropStat :: Statement -> Iter Block (StateIO ConstMap) Block
constPropStat (Assignment nm (ExprConstant x)) = do
   modify ((nm, x) :)
   return []
constPropStat e@(Assignment nm (ExprVar vNm)) = do
   xs <- get
   case lookup vNm xs of
     Just x  -> constPropStat (Assignment nm (ExprConstant x))
     Nothing -> return [e]
constPropStat e@(Return (ExprVar vNm)) = do
   xs <- get
   case lookup vNm xs of
     Just x  -> return [Return (ExprConstant x)]
     Nothing -> return [e]
constPropStat x = return [x]

