{-# LANGUAGE Trustworthy #-}

module Main where

import System.Environment
  ( getArgs
  )
import ArgParser
  ( parseArguments
  , Cfg
  , optPasses
  , inFile
  , outFile
  , optPasses
  , isText
  )
import Opt
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

main :: IO ()
main = do
    xs <- getArgs 
    compile $ parseArguments optPassNames (unwords xs)

compile :: Cfg -> IO ()
compile cfg
  | cfg^.isText == False = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
  | otherwise = do
    out <- output $ cfg^.outFile
    input (cfg^.inFile) |. parseAndPrint (cfg^.optPasses) |$ out

input :: FilePath -> Onum L.ByteString IO a
input "-" = enumStdin
input p   = enumFile' p

output :: FilePath -> IO (Iter L.ByteString IO ())
output "-" = return stdoutI
output p   = handleI `fmap` openFile p WriteMode

