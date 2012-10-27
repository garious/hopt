{-# LANGUAGE Trustworthy #-}  -- Can't have both Safe Haskell and Template Haskell
{-# LANGUAGE TemplateHaskell #-}

module ArgParser where

-- From lens package
import Control.Lens.TH
  ( makeLenses
  )
import Control.Lens.Setter
  ( (%=)   -- modify a lens from a monad
  , (.=)   -- assign a lens from a monad
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
  , ChunkData
  , (|$)
  , enumPure
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

data Cfg = Cfg {
    _optPasses  :: [String]
  , _inFile     :: FilePath
  , _outFile    :: FilePath
  , _isText     :: Bool      -- Output as text, not bitcode
  } deriving (Show, Eq)

-- Use Template Haskell to generate getters and setters
-- for each field in Cfg
makeLenses ''Cfg

-- By default, do no optimizations on stdin, compile to bitcode and output to stdout
defaultCfg :: Cfg
defaultCfg = Cfg [] "-" "-" False

parseArguments :: [String] -> String -> Cfg
parseArguments optPassNames = flip execState defaultCfg . argParser
  where
    argParser :: String -> State Cfg ()
    argParser s = enumPure s |$ arguments

    arguments :: Iter String (State Cfg) ()
    arguments = skipMany argument <* eofI

    argument :: Iter String (State Cfg) ()
    argument = textOutputFlag
           <|> outputFileFlag
           <|> optPassFlag optPassNames
           <|> inputFileArg

textOutputFlag :: Iter String (State Cfg) ()
textOutputFlag = do
    flag "S"
    isText .= True

outputFileFlag :: Iter String (State Cfg) ()
outputFileFlag = do
    flag "o"
    s <- flagArg
    outFile .= s

inputFileArg :: Iter String (State Cfg) ()
inputFileArg = do
    s <- argArg
    inFile .= s

optPassFlag :: [String] -> Iter String (State Cfg) ()
optPassFlag optPassNames = do
    p <- string "-" *> flagArg
    if elem p optPassNames
      then optPasses %= (++ [p])
      else mzero

flag :: Monad m => String -> Iter String m ()
flag s = string "-" *> string s *> skipSpace

skipSpace :: (ListLike t e, ChunkData t, Eq e, Enum e, Monad m) => Iter t m ()
skipSpace = skipWhileI (\c -> c == eord ' ' || c == eord '\t')

argArg :: Monad a => Iter String a String
argArg = satisfy (/= eord '-') <:> whileI (/= eord ' ') <* skipSpace

flagArg :: Monad a => Iter String a String
flagArg = while1I (/= eord ' ') <* skipSpace

