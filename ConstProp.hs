{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constant Propagation
--
--   This optimization pass evaluates constant expressions, and
--   removes assignments where the right-hand side is a constant.

module ConstProp where

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

type ConstMap = [(String, Literal)]

data PassState = S {
    currLabel   :: String
  , constMap    :: ConstMap
  , constMapMap :: [(String, ConstMap)]
  , notFlushed  :: ConstMap
  }

name                                  :: String
name                                   = "constprop"

emptyState                            :: PassState
emptyState                             = S "" [] [] []

chunk                                 :: MonadState PassState m => Module -> m Module
chunk                                  = mapM toplevelEntity

toplevelEntity                          :: MonadState PassState m => ToplevelEntity -> m ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           put emptyState
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x

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
                                           return [Assignment nm (ExprConstant lit) | (nm, lit) <- notFlushed st]
stat (Branch s)                        = return [Branch s]
stat (BranchCond e b1 b2)              = do
                                           e' <- expr e
                                           return $ case e' of
                                             ExprConstant (LitBool True)  -> [Branch b1]
                                             ExprConstant (LitBool False) -> [Branch b2]
                                             _ -> [BranchCond e' b1 b2]

expr                                  :: MonadState PassState m => Expr -> m Expr
expr                                   = transformM expr'

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

addConst                              :: MonadState PassState m => String -> Literal -> m ()
addConst nm x                          = modify $ \st -> st{
                                           constMap   = (nm, x) : constMap st
                                         , notFlushed = (nm, x) : notFlushed st
                                         }

