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
    _optPasses  :: [String]
  , _inFile     :: FilePath
  , _outFile    :: FilePath
  , _isText     :: Bool      -- Output as text, not bitcode
  } deriving (Show, Eq)

-- Use Template Haskell to generate getters and setters
-- for each field in Cfg
mkLabel ''Cfg

type OptPass = Iter Flow IO Flow

main :: IO ()
main = do
    xs <- getArgs 
    compile $ parseArguments (unwords xs)

compile :: Cfg -> IO ()
compile Cfg{_isText=False} = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
compile cfg  = do
    hdl <- outPath == "-" ? (return stdout, openFile outPath WriteMode)
    input |$ parseFlow .| optimize (get optPasses cfg) .| printFlow .| handleI hdl
  where
    input = inPath == "-" ? (enumStdin, enumFile' inPath)
    inPath  = get inFile cfg
    outPath = get outFile cfg

data Flow = Nop
          | BasicBlock [Char]
  deriving (Show)

instance Monoid Flow where
    mempty                                = Nop
    mappend Nop x                         = x
    mappend x Nop                         = x
    mappend (BasicBlock x) (BasicBlock y) = BasicBlock $ x ++ y

instance ChunkData Flow where
    null Nop     = True
    null _       = False
    chunkShow    = show

parseFlow :: Inum L.ByteString Flow IO a
parseFlow = mkInum parseFlow'

parseFlow' :: Iter L.ByteString IO Flow
parseFlow' = lineI >>= return . BasicBlock . L.unpack

optimize :: [String] -> Inum Flow Flow IO a
optimize []     = inumNop
optimize (x:xs) = maybe (error x) mkInum (lookup x optPassMap) |. optimize xs

printFlow :: Inum Flow L.ByteString IO a
printFlow = mkInum prettyFlow

prettyFlow :: Iter Flow IO L.ByteString
prettyFlow  = dataI >>= return . L.pack . (++"\n") . show

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
       <|> optPassFlag
       <|> inputFileArg

textOutputFlag :: Iter String (State Cfg) ()
textOutputFlag = flag "S" >> puts isText True

outputFileFlag :: Iter String (State Cfg) ()
outputFileFlag = flag "o" >> flagArg >>= puts outFile

inputFileArg :: Iter String (State Cfg) ()
inputFileArg = argArg >>= puts inFile

optPassFlag :: Iter String (State Cfg) ()
optPassFlag = do
    p <- string "-" *> flagArg
    case lookup p optPassMap of
       Just _  -> modify optPasses (++ [p])
       Nothing -> mzero

-- | A map from command-line names to the function that implements an
--   optimization pass
optPassMap :: [(String, OptPass)]
optPassMap = [
    ("constprop", constProp)
  ]

-- \ The Constant Propogation pass
constProp :: OptPass
constProp = dataI

flag :: Monad m => String -> Iter String m ()
flag s = string "-" *> string s *> skipSpace

skipSpace :: (ListLike t e, ChunkData t, Eq e, Enum e, Monad m) => Iter t m ()
skipSpace = skipWhileI (\c -> c == eord ' ' || c == eord '\t')

argArg :: Monad a => Iter String a String
argArg = satisfy (/= eord '-') <:> whileI (/= eord ' ') <* skipSpace

flagArg :: Monad a => Iter String a String
flagArg = while1I (/= eord ' ') <* skipSpace

