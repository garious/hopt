{-# LANGUAGE Trustworthy #-}

-- | The core of the optimization framework.  This module combines each
--   incremental stream processor.  This includes the parser, the
--   optimizers, and the pretty-printer.

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

-- | A pipe that accepts an LLVM ByteString and pushes an optimized ByteString.
--   'p' is the filename, and 'xs' are the names of optimization passes to run.
parseAndPrint :: FilePath -> [String] -> Inum L.ByteString L.ByteString IO a
parseAndPrint p xs = parseLlvm |. optimize xs |. printLlvm p

-- | A pipe that accepts an LLVM ByteString and pushes a list of top-level LLVM
--   entities, such as targets or function definitions.
parseLlvm :: Inum L.ByteString Module IO a
parseLlvm = mkInum $ atto toplevelEntities

-- | A pipe that accepts a list of LLVM top-level entities and pushes an LLVM
--   ByteString.
printLlvm :: FilePath -> Inum Module L.ByteString IO a
printLlvm p = mkInum $ (L.unlines . (moduleId p :) . map toLlvm) `fmap` dataI

-- | Generate a Module ID statement for the given filepath.
moduleId :: FilePath -> L.ByteString
moduleId p = L.pack $ "; ModuleID = '" ++ name p ++ "'"
  where
    name "-" = "<stdin>"
    name x   = x

-- | A pipe that takes a list of top-level entities and pushes the same list,
--   but after having optimized those entities.
optimize :: [String] -> Inum Module Module IO a
optimize = foldr (|.) inumNop . map lookupPass

-- | Given the name of an optimization pass, returns the pipe that implements
--   that requested optimization.
lookupPass :: String -> Inum Module Module IO a
lookupPass x = maybe (error x) id (lookup x optPassMap)

-- | The list of known optimization passes.
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

-- | A utility function creating stateful optimization passes.  It takes
--   2 parameters, an iterator for a chunk of top-level entities, and the
--   state to pass into the first chunk.  The function will run the
--   optimization, collect the output state and then feed it back into the
--   next chunk processor when the next chunk enters the pipeline.
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

