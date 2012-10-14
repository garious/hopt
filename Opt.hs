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
  , (|$)
  , (|.)
  , (.|)
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
    input (get inFile cfg) |$ parseFlow 
                          .| optimize (get optPasses cfg)
                          .| printFlow
                          .| out

input :: FilePath -> Onum L.ByteString IO a
input "-" = enumStdin
input p   = enumFile' p

output :: FilePath -> IO (Iter L.ByteString IO ())
output "-" = return stdoutI
output p   = handleI `fmap` openFile p WriteMode

optimize :: [String] -> Inum Block Block IO a
optimize []     = inumNop
optimize (x:xs) = maybe (error x) id (lookup x optPassMap) |. optimize xs

printFlow :: Inum Block L.ByteString IO a
printFlow = mkInum $ do
   xs <- dataI
   return $ L.unlines (map pretty xs)

optPassNames :: [String]
optPassNames = map fst optPassMap

-- | A map from command-line names to the function that implements an
--   optimization pass
optPassMap :: [(String,  Inum Block Block IO a)]
optPassMap = [
    ("constprop", constProp)
  , ("unassigned", unassignedVars)
  ]
