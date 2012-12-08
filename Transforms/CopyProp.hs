{-# LANGUAGE Safe #-}

-- | Copy Propagation
--
--   This optimization pass removes redundant assignments

module Transforms.CopyProp where

import Control.Monad.Trans.State
  ( modify
  , get
  , put
  , StateT
  )
import Control.Lens.Plated
  ( transformM
  )
import LlvmData

-- | Pass state.  A list of variable aliases.
type PassState                         = [(String, String)]

-- | Name of this optimization pass
name                                  :: String
name                                   = "copyprop"

-- | Initial pass state
emptyState                            :: PassState
emptyState                             = []

-- | Optimizes top-level entities
chunk                                 :: Monad m => Module -> StateT PassState m Module
chunk                                  = mapM toplevelEntity

-- | Optimizes a top-level entity
toplevelEntity                        :: Monad m => ToplevelEntity -> StateT PassState m ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           put []
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x


-- | Optimizes a statement
stat                                  :: Monad m => Statement -> StateT PassState m Block
stat (Assignment silly e)              = do
                                           e' <- expr e
                                           case e' of
                                             ExprVar nm -> pushAlias silly nm >> return []
                                             _          -> return [Assignment silly e']
stat (Return ty e)                     = do
                                           e' <- expr e
                                           return [Return ty e']
stat Flush                             = return []
stat x                                 = return [x]


-- | Bottom-up optimization of each expression
expr                                  :: Monad m => Expr -> StateT PassState m Expr
expr                                   = transformM expr'

-- | Optimizes an expression
expr'                                 :: Monad m => Expr -> StateT PassState m Expr
expr' (ExprVar nm)                     = popAlias nm
expr' x                                = return x


-- | Push a register alias into the pass state
pushAlias                             :: Monad m => String -> String -> StateT PassState m ()
pushAlias nm x                         = modify ((nm, x) :)

-- | Get the best name for the given register
popAlias                              :: Monad m => String -> StateT PassState m Expr
popAlias nm                            = do
                                           xs <- get
                                           return $ ExprVar (maybe nm id (lookup nm xs))

