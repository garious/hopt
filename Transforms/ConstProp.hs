{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constant Propagation
--
--   This optimization pass evaluates constant expressions, and
--   removes assignments where the right-hand side is a constant.

module Transforms.ConstProp where

import Control.Monad.State
  ( liftM
  , modify
  , get
  , put
  )
import Control.Monad.State.Class
  ( MonadState
  )
import Control.Lens.Plated
  ( transformM
  )
import LlvmData

-- | A lookup table from a constant's name to its literal value.
type ConstMap = [(String, Literal)]

-- | The State for the ConstProp pass
data PassState = S {
    currLabel   :: String
  , constMap    :: ConstMap
  , constMapMap :: [(String, ConstMap)]
  , notFlushed  :: ConstMap
  }

-- | Name of this optimization pass
name                                  :: String
name                                   = "constprop"

-- | Initial state for this optimization pass
emptyState                            :: PassState
emptyState                             = S "" [] [] []

-- | Optimize a chunk of top-level entities
chunk                                 :: MonadState PassState m => Module -> m Module
chunk                                  = mapM toplevelEntity

-- | Optimize a top-level entity
toplevelEntity                          :: MonadState PassState m => ToplevelEntity -> m ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           put emptyState
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x
 
-- | Optimize a statement
stat                                  :: MonadState PassState m => Statement -> m Block
stat (Assignment nm e)                 = do
                                            e' <- expr e
                                            case e' of
                                              ExprConstant x -> addConst nm x >> return []
                                              x              -> return [Assignment nm x]
stat (Label s)                         = do
                                           modify $ \st -> st{
                                             currLabel = s
                                           , constMap  = []
                                           , constMapMap = (currLabel st, constMap st) : constMapMap st
                                           }
                                           return [Label s]
stat (Return s e)                      = do
                                           e' <- expr e
                                           return [Return s e']
stat Flush                             = do
                                           st <- get
                                           put st{notFlushed = []}
                                           return [Assignment nm (ExprConstant lit) | (nm, lit) <- reverse (notFlushed st)]
stat (Branch s)                        = return [Branch s]
stat (BranchCond e b1 b2)              = do
                                           e' <- expr e
                                           return $ case e' of
                                             ExprConstant (LitBool True)  -> [Branch b1]
                                             ExprConstant (LitBool False) -> [Branch b2]
                                             _ -> [BranchCond e' b1 b2]

-- | Do a bottom-up traversal and optimize each expression
expr                                  :: MonadState PassState m => Expr -> m Expr
expr                                   = transformM expr'

-- | Optimize an expression
expr'                                 :: MonadState PassState m => Expr -> m Expr
expr' (ExprVar nm)                     = do
                                           xs <- liftM constMap get
                                           return $ maybe (ExprVar nm) ExprConstant (lookup nm xs)
expr' (ExprAdd _ty (ExprConstant (LitInteger i1)) (ExprConstant (LitInteger i2)))
                                       = return $ ExprConstant (LitInteger (i1 + i2))
expr' (ExprPhi ty (x:xs))              = do
                                           x'  <- phiField x
                                           xs' <- mapM phiField xs
                                           return $ if all (== fst x') (map fst xs')
                                                      then fst x'
                                                      else ExprPhi ty (x':xs')
expr' e                                = return e

-- | Optimize a phi field
phiField                              :: MonadState PassState m => (Expr, String) -> m (Expr, String)
phiField (e, lbl)                      = do
                                           st <- get
                                           case lookup lbl (constMapMap st) of
                                             Just cMap -> do
                                                put st{constMap = cMap}
                                                e' <- expr e
                                                put st
                                                return (e', lbl)
                                             Nothing -> do
                                                return (e, lbl)

-- | Add a contant to the optimization pass state
addConst                              :: MonadState PassState m => String -> Literal -> m ()
addConst nm x                          = modify $ \st -> st{
                                           constMap   = (nm, x) : constMap st
                                         , notFlushed = (nm, x) : notFlushed st
                                         }

