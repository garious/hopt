{-# LANGUAGE Safe #-}

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

main :: IO ()
main = do
    xs <- getArgs 
    case parseArguments optPassNames (unwords xs) of
      Left err  -> error (show err)
      Right cfg -> compile cfg

compile :: Cfg -> IO ()
compile cfg
  | cfg^.isText == False = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
  | otherwise = do
    out <- output $ cfg^.outFile
    input (cfg^.inFile) |. parseAndPrint (cfg^.inFile) (cfg^.optPasses) |$ out

input :: FilePath -> Onum L.ByteString IO a
input "-" = enumStdin
input p   = enumFile' p

output :: FilePath -> IO (Iter L.ByteString IO ())
output "-" = return stdoutI
output p   = handleI `fmap` openFile p WriteMode

