{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}

module CopyProp where

-- | Copy Propagation
--
--   This optimization pass removes redundant assignments

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

type PassState = [(String, String)] -- A list of variable aliases.

name                                  :: String
name                                   = "copyprop"

emptyState                            :: PassState
emptyState                             = []

chunk                                 :: MonadState PassState m => Module -> m Module
chunk                                  = mapM toplevelEntity

toplevelEntity                        :: MonadState PassState m => ToplevelEntity -> m ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           put []
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x

stat                                  :: MonadState PassState m => Statement -> m Block
stat (Assignment silly e)              = do
                                           e' <- expr e
                                           case e' of
                                             ExprVar nm -> pushAlias silly nm >> return []
                                             _          -> return [Assignment silly e']
stat (Return ty e)                     = do
                                           e' <- expr e
                                           return [Return ty e']
stat x                                 = return [x]

expr                                  :: MonadState PassState m => Expr -> m Expr
expr                                   = transformM expr'

expr'                                 :: MonadState PassState m => Expr -> m Expr
expr' (ExprVar nm)                     = popAlias nm
expr' x                                = return x


pushAlias                             :: MonadState PassState m => String -> String -> m ()
pushAlias nm x                         = modify ((nm, x) :)

popAlias                              :: MonadState PassState m => String -> m Expr
popAlias nm                            = do
                                           xs <- get
                                           return $ ExprVar (maybe nm id (lookup nm xs))

