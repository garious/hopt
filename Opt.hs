-- {-# LANGUAGE Safe #-}  -- Can't have both Safe Haskell and Template Haskell
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (null)
import System.Environment
  ( getArgs
  )
import System.IO
  ( openFile
  , IOMode(WriteMode)
  , stdout
  )
import Data.Monoid
  ( mempty
  , mappend
  , Monoid
  )

-- From fclabels package
import Data.Label
  ( mkLabel
  , get
  )
import Data.Label.PureM
  ( puts
  , modify
  )

-- From mtl package
import Control.Monad.State
  ( State
  , mzero
  , execState
  )

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , ChunkData
  , chunkShow
  , (|$)
  , (|.)
  , (.|)
  , enumPure
  , enumFile'
  , enumStdin
  , mkInum
  , handleI
  , lineI
  , inumNop
  , dataI
  )
import Data.IterIO.Iter
  ( null
  )
import Data.IterIO.Parse
  ( string
  , (<|>)
  , (<:>)
  , (<*)
  , (*>)
  , skipMany
  , satisfy
  , eofI
  , eord
  , whileI
  , while1I
  , skipWhileI
  )
import Data.ListLike
  ( ListLike
  )

import qualified Data.ByteString.Lazy.Char8 as L

data Cfg = Cfg {
    _passes  :: [String]
  , _inFile  :: FilePath
  , _outFile :: FilePath
  , _isText  :: Bool      -- Output as text, not bitcode
  } deriving (Show, Eq)

-- Use Template Haskell to generate getters and setters
-- for each field in Cfg
mkLabel ''Cfg

type Pass = Iter Llvm IO Llvm

main :: IO ()
main = do
    xs <- getArgs 
    compile $ parseArguments (unwords xs)

compile :: Cfg -> IO ()
compile Cfg{_isText=False} = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
compile cfg  = do
    hdl <- outPath == "-" ? (return stdout, openFile outPath WriteMode)
    input |$ parseLlvm .| optimize (get passes cfg) .| printLlvm .| handleI hdl
  where
    input = inPath == "-" ? (enumStdin, enumFile' inPath)
    inPath  = get inFile cfg
    outPath = get outFile cfg

data Llvm = LlvmNop
          | BasicBlock [Char]
  deriving (Show)

instance Monoid Llvm where
    mempty                                = LlvmNop
    mappend LlvmNop x                     = x
    mappend x LlvmNop                     = x
    mappend (BasicBlock x) (BasicBlock y) = BasicBlock $ x ++ y

instance ChunkData Llvm where
    null LlvmNop = True
    null _       = False
    chunkShow    = show

parseLlvm :: Inum L.ByteString Llvm IO a
parseLlvm = mkInum parseLlvm'

parseLlvm' :: Iter L.ByteString IO Llvm
parseLlvm' = lineI >>= return . BasicBlock . L.unpack

optimize :: [String] -> Inum Llvm Llvm IO a
optimize []     = inumNop
optimize (x:xs) = maybe (error x) mkInum (lookup x passMap) |. optimize xs

printLlvm :: Inum Llvm L.ByteString IO a
printLlvm = mkInum prettyLlvm

prettyLlvm :: Iter Llvm IO L.ByteString 
prettyLlvm  = dataI >>= return . L.pack . (++"\n") . show

-- | An alternative to the if-then-else syntax
(?) :: Bool -> (a, a) -> a
b ? (t, e) = if b then t else e
infixl 1 ?

parseArguments :: String -> Cfg
parseArguments = flip execState defaultCfg . argParser

-- By default, do no optimizations on stdin, compile to bitcode and output to stdout
defaultCfg :: Cfg
defaultCfg = Cfg [] "-" "-" False

argParser :: String -> State Cfg ()
argParser s = enumPure s |$ arguments

arguments :: Iter String (State Cfg) ()
arguments = skipMany argument <* eofI

argument :: Iter String (State Cfg) ()
argument = textOutputFlag
       <|> outputFileFlag
       <|> passFlag
       <|> inputFileArg

textOutputFlag :: Iter String (State Cfg) ()
textOutputFlag = flag "S" >> puts isText True

outputFileFlag :: Iter String (State Cfg) ()
outputFileFlag = flag "o" >> flagArg >>= puts outFile

inputFileArg :: Iter String (State Cfg) ()
inputFileArg = argArg >>= puts inFile

passFlag :: Iter String (State Cfg) ()
passFlag = do
    p <- string "-" *> flagArg
    case lookup p passMap of
       Just _  -> modify passes (++ [p])
       Nothing -> mzero

-- | A map from command-line names to the function that implements an
--   optimization pass
passMap :: [(String, Pass)]
passMap = [
    ("constprop", constProp)
  ]

-- \ The Constant Propogation pass
constProp :: Pass
constProp = dataI

flag :: Monad m => String -> Iter String m ()
flag s = string "-" *> string s *> skipSpace

skipSpace :: (ListLike t e, ChunkData t, Eq e, Enum e, Monad m) => Iter t m ()
skipSpace = skipWhileI (\c -> c == eord ' ' || c == eord '\t')

argArg :: Monad a => Iter String a String
argArg = satisfy (/= eord '-') <:> whileI (/= eord ' ') <* skipSpace

flagArg :: Monad a => Iter String a String
flagArg = while1I (/= eord ' ') <* skipSpace

