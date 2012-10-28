{-# LANGUAGE Safe #-}

module Opt where


-- From IterIO package
import Data.IterIO
  ( Inum
  , (|.)
  , inumNop
  )

import ConstProp
  ( constProp
  )
--import UnassignedVars
--  ( unassignedVars
--  )
import Block
  ( Module
  )
import LlvmParser
  ( parseFlow
  )
import ToLlvm
  ( printFlow
  )

import qualified Data.ByteString.Lazy.Char8 as L

parseAndPrint :: [String] -> Inum L.ByteString L.ByteString IO a
parseAndPrint xs = parseFlow |. optimize xs |. printFlow

optimize :: [String] -> Inum Module Module IO a
optimize = foldr (|.) inumNop . map lookupPass

lookupPass :: String -> Inum Module Module IO a
lookupPass x = maybe (error x) id (lookup x optPassMap)

optPassNames :: [String]
optPassNames = map fst optPassMap

-- | A map from command-line names to the function that implements an
--   optimization pass
optPassMap :: [(String,  Inum Module Module IO a)]
optPassMap = [
    ("constprop", constProp)
  --, ("unassigned", unassignedVars)
  ]
