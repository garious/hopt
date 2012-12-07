{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Dead Instruction Elimination
--
--   This optimization pass removes assignment statements that are 
--   unreferenced by all later statements.  Unlike the LLVM version,
--   this version also moves each instruction as closely as possible to
--   the first instruction that references it.
--
--   How it works:
--
--   When a definition (i.e. assignment) is parsed, we pull it out of the basic
--   block until another statement references it.  Then, for each statement, we
--   lookup all definitions it uses and check for them in our list of
--   unreferenced definitions.  If we find one, we push the definition back
--   into the basic block, just before the statement that references it.
--
--   For example:
--
--   Line  Input         PassState          Output             Comment
--   ----  --------      ---------          ------             -------
--     1   %1 = 5        [%1 = 5]                              %1 may be unreferrenced
--     2   %2 = 7        [%1 = 5, %2 = 7]                      %1 & %2 may be unreferrenced
--     3   %3 = %1       [%2 = 7, %3 = %1]  [%1 = 5]           %1 is used
--     4   ret %3        [%2 = 7]           [%3 = %1, ret %3]  %3 is used, %2 is dropped
--
--   Limitations:
--
--      1. If there is a chain of unreferenced definitions, this pass will only
--         remove the first link in the chain.
--      2. Code size: A redundant definition can be pulled into branch if both
--         sides reference the definition for the first time.
--
--   Hindsight:
--
--      This pass is probably more useful in improving spacial locality and
--      reducing register pressure than removing dead instructions.
--

module Transforms.DeadInstructionElimination where

import Data.List
  ( delete
  )
import Control.Monad.State
  ( modify
  , get
  , put
  )
import Control.Monad.State.Class
  ( MonadState
  )
import Control.Lens.Plated
  ( universe
  )
import LlvmData

-- | A list of unreferenced instructions.
--   The String is a variable name, and
--   the Expr is the value assigned to it.
type PassState = [(String, Expr)]

-- | The name of this optimization pass
name                                  :: String
name                                   = "die"

-- | The initial state
emptyState                            :: PassState
emptyState                             = []

-- | Optimize a list of top-level entities
chunk                                 :: MonadState PassState m => Module -> m Module
chunk                                  = mapM toplevelEntity

-- | Optimize a top-level entity
toplevelEntity                        :: MonadState PassState m => ToplevelEntity -> m ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           put []
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x


-- | Optimize a statement
stat                                  :: MonadState PassState m => Statement -> m Block
stat (Assignment nm e)                 = do
                                           xss <- mapM popStatement (vars e)
                                           pushStatement nm e
                                           return $ concat xss
stat s@(Return _ e)                    = do
                                           xss <- mapM popStatement (vars e)
                                           return $ concat xss ++ [s]
stat Flush                             = do
                                           st <- get
                                           put []
                                           return $ map (uncurry Assignment) st
stat x                                 = return [x]


-- | Given an expression, return the variable names it references
vars                                  :: Expr -> [String]
vars                                   = concatMap vars' . universe

-- | Given an expresion, if it is variable, return its name.  Otherwise, return an empty list
vars'                                 :: Expr -> [String]
vars' (ExprVar nm)                     = [nm]
vars' _                                = []


-- | Push a statement that may be dead
pushStatement                         :: MonadState PassState m => String -> Expr -> m ()
pushStatement nm x                     = modify ((nm, x) :)


-- | Lookup a statement that is now known to be undead.  Remove it from the list of dead instructions.
popStatement                          :: MonadState PassState m => String -> m [Statement]
popStatement nm                          = do 
                                             xs <- get
                                             case lookup nm xs of
                                               Just x -> do
                                                 modify (delete (nm,x))
                                                 return [Assignment nm x]
                                               Nothing -> return []

