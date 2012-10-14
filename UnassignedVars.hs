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
unassignedVars = statefulPass chunk []

chunk :: Iter Block (StateT UnassignedVars IO) Block
chunk = concat `fmap` (dataI >>= mapM stat)

stat :: Statement -> Iter Block (StateT UnassignedVars IO) Block
stat e@(Declaration _ nm) = do
   modify ((nm, e) :)
   return []
stat e@(Assignment nm _) = do
   xs <- get
   case lookup nm xs of
     Just decl -> modify (delete (nm, e)) >> return [decl, e]
     Nothing -> return [e]
stat x = return [x]

