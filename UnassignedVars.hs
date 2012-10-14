{-# LANGUAGE Safe #-}

module UnassignedVars where

import Data.IterIO
  ( Iter
  , Inum
  , dataI
  )
import Data.List
  ( delete
  )
import Control.Monad.State
  ( StateT
  , modify
  , get
  )
import OptPassUtils
  ( statefulPass
  )
import Block

type UnassignedVars = [(String, Statement)]

-- \ The Unused Variables pass
--   Move the declaration down to just before the variable's first assignment.
--   If it is never assigned, it is discarded.
unassignedVars :: Inum Block Block IO a
unassignedVars = statefulPass unassignedVarsChunk []

unassignedVarsChunk :: Iter Block (StateT UnassignedVars IO) Block
unassignedVarsChunk = concat `fmap` (dataI >>= mapM unassignedVarsStat)

unassignedVarsStat :: Statement -> Iter Block (StateT UnassignedVars IO) Block
unassignedVarsStat e@(Declaration _ nm) = do
   modify ((nm, e) :)
   return []
unassignedVarsStat e@(Assignment nm _) = do
   xs <- get
   case lookup nm xs of
     Just decl -> modify (delete (nm, e)) >> return [decl, e]
     Nothing -> return [e]
unassignedVarsStat x = return [x]

