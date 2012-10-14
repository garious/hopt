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
  )

-- From fclabels package
import Data.Label.Pure
  ( get
  )

-- From IterIO package
import Data.IterIO
  ( Inum
  , Onum
  , Iter
  , IterR
  , (|$)
  , (|.)
  , enumFile'
  , enumStdin
  , stdoutI
  , mkInum
  , handleI
  , inumNop
  , dataI
  )

import ConstProp
  ( constProp
  )
import UnassignedVars
  ( unassignedVars
  )
import Block
  ( Block
  , pretty
  )
import TsParser
  ( parseFlow
  )

import qualified Data.ByteString.Lazy.Char8 as L

compile :: Cfg -> IO ()
compile Cfg{_isText=False} = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
compile cfg  = do
    out <- output (get outFile cfg)
    input (get inFile cfg) |. parseAndPrint (get optPasses cfg) |$ out

parseAndPrint :: [String] -> Iter L.ByteString IO a -> Iter L.ByteString IO (IterR L.ByteString IO a)
parseAndPrint xs = parseFlow |. optimize xs |. printFlow

input :: FilePath -> Onum L.ByteString IO a
input "-" = enumStdin
input p   = enumFile' p

output :: FilePath -> IO (Iter L.ByteString IO ())
output "-" = return stdoutI
output p   = handleI `fmap` openFile p WriteMode

optimize :: [String] -> Inum Block Block IO a
optimize = foldr (|.) inumNop . map lookupPass

lookupPass :: String -> Inum Block Block IO a
lookupPass x = maybe (error x) id (lookup x optPassMap)

printFlow :: Inum Block L.ByteString IO a
printFlow = mkInum $ (L.unlines . map pretty) `fmap` dataI

optPassNames :: [String]
optPassNames = map fst optPassMap

-- | A map from command-line names to the function that implements an
--   optimization pass
optPassMap :: [(String,  Inum Block Block IO a)]
optPassMap = [
    ("constprop", constProp)
  , ("unassigned", unassignedVars)
  ]
