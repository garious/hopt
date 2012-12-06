{-# LANGUAGE Safe #-}

-- | An implementation of a very small subset of LLVM's opt

module Main where

import System.Environment
  ( getArgs
  )
import HoptArgParser
  ( parseArguments
  )
import HoptArgData
  ( Cfg
  , optPasses
  , inFile
  , outFile
  , optPasses
  , isText
  )
import Hopt
  ( parseAndPrint
  , optPassNames
  )
import System.IO
  ( openFile
  , IOMode(WriteMode)
  )
-- From IterIO package
import Data.IterIO
  ( Onum
  , Iter
  , (|$)
  , (|.)
  , enumFile'
  , enumStdin
  , stdoutI
  , handleI
  )
-- From lens package
import Control.Lens.Getter
  ( (^.) -- Getter
  )

import qualified Data.ByteString.Lazy.Char8 as L

-- | The entry point for hopt
main :: IO ()
main = do
    xs <- getArgs 
    case parseArguments optPassNames (unwords xs) of
      Left err  -> error (show err)
      Right cfg -> compile cfg

-- | Given a configuration, compile the input file and write the output file
compile :: Cfg -> IO ()
compile cfg
  | cfg^.isText == False = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
  | otherwise = do
    out <- output $ cfg^.outFile
    input (cfg^.inFile) |. parseAndPrint (cfg^.inFile) (cfg^.optPasses) |$ out

-- | The input pipe that enumerates the input file.  If 'p' is a single hyphen, stdin is used.
input :: FilePath -> Onum L.ByteString IO a
input "-" = enumStdin
input p   = enumFile' p

-- | The output iterator that writes the output file.  If 'p' is a single hyphen, stdout is used.
output :: FilePath -> IO (Iter L.ByteString IO ())
output "-" = return stdoutI
output p   = handleI `fmap` openFile p WriteMode

