{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Copy Propagation
--
--   This optimization pass removes redundant assignments

module Transforms.CopyProp where

import Control.Monad.State
  ( modify
  , get
  , put
  )
import Control.Monad.State.Class
  ( MonadState
  )
import Control.Lens
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
chunk                                 :: MonadState PassState m => Module -> m Module
chunk                                  = mapM toplevelEntity

-- | Optimizes a top-level entity
toplevelEntity                        :: MonadState PassState m => ToplevelEntity -> m ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           put []
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x


-- | Optimizes a statement
stat                                  :: MonadState PassState m => Statement -> m Block
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
expr                                  :: MonadState PassState m => Expr -> m Expr
expr                                   = transformM expr'

-- | Optimizes an expression
expr'                                 :: MonadState PassState m => Expr -> m Expr
expr' (ExprVar nm)                     = popAlias nm
expr' x                                = return x


-- | Push a register alias into the pass state
pushAlias                             :: MonadState PassState m => String -> String -> m ()
pushAlias nm x                         = modify ((nm, x) :)

-- | Get the best name for the given register
popAlias                              :: MonadState PassState m => String -> m Expr
popAlias nm                            = do
                                           xs <- get
                                           return $ ExprVar (maybe nm id (lookup nm xs))

