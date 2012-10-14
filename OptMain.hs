{-# LANGUAGE Safe #-}

module Main where

import System.Environment
  ( getArgs
  )
import ArgParser
  ( parseArguments
  , Cfg(Cfg)
  , optPasses
  , inFile
  , outFile
  , optPasses
  , _isText
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

-- From fclabels package
import Data.Label.Pure
  ( get
  )

import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    xs <- getArgs 
    compile $ parseArguments optPassNames (unwords xs)

compile :: Cfg -> IO ()
compile Cfg{_isText=False} = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
compile cfg  = do
    out <- output (get outFile cfg)
    input (get inFile cfg) |. parseAndPrint (get optPasses cfg) |$ out

input :: FilePath -> Onum L.ByteString IO a
input "-" = enumStdin
input p   = enumFile' p

output :: FilePath -> IO (Iter L.ByteString IO ())
output "-" = return stdoutI
output p   = handleI `fmap` openFile p WriteMode

