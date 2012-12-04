{-# LANGUAGE Trustworthy #-}

module Hopt where

import Data.IterIO
  ( Inum
  , Iter
  , ChunkData
  , (|.)
  , inumNop
  , mkInum
  , mkInumAutoM
  , ifeed
  , dataI
  , ipopresid
  , ungetI
  )
import Data.IterIO.Trans
  ( runStateTLI
  , liftI
  )
import Data.IterIO.Atto
  ( atto
  )
import Control.Monad.State
  ( StateT
  )
import qualified Transforms.ConstProp as ConstProp
import qualified Transforms.CopyProp as CopyProp
import qualified Transforms.DeadInstructionElimination as DIE
import LlvmData
  ( Module
  )
import LlvmParser
  ( toplevelEntities
  )
import LlvmPrinter
  ( toLlvm
  )

import qualified Data.ByteString.Lazy.Char8 as L

parseAndPrint :: FilePath -> [String] -> Inum L.ByteString L.ByteString IO a
parseAndPrint p xs = parseFlow |. optimize xs |. printFlow p

parseFlow :: Inum L.ByteString Module IO a
parseFlow = mkInum $ atto toplevelEntities

printFlow :: FilePath -> Inum Module L.ByteString IO a
printFlow p = mkInum $ (L.unlines . (moduleId p :) . map toLlvm) `fmap` dataI

moduleId :: FilePath -> L.ByteString
moduleId p = L.pack $ "; ModuleID = '" ++ name p ++ "'"
  where
    name "-" = "<stdin>"
    name x   = x

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
    (ConstProp.name, statefulPass ConstProp.chunk ConstProp.emptyState)
  , ( CopyProp.name, statefulPass  CopyProp.chunk  CopyProp.emptyState)
  , (      DIE.name, statefulPass       DIE.chunk       DIE.emptyState)
  ]

statefulPass :: (Monad m, ChunkData tOut) => (tOut -> Iter tOut (StateT s m) tOut) -> s -> Inum tOut tOut m a
statefulPass iter = mkInumAutoM . loop
  where 
    loop st = do
      x <- dataI
      (t', st') <- liftI $ runStateTLI (iter x) st
      done <- ifeed t'
      if not done
        then loop st'
        else ipopresid >>= ungetI

