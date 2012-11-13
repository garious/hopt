{-# LANGUAGE Safe #-}

module ConstProp where

import Data.IterIO
  ( Iter
  , Inum
  , dataI
  )
import Control.Monad.State
  ( StateT
  , modify
  , get
  , put
  )
import OptPassUtils
  ( statefulPass
  )
import Block

type ConstMap = [(String, Literal)]

data PassState = S {
    currLabel   :: String
  , constMap    :: ConstMap
  , constMapMap :: [(String, ConstMap)]
  , notFlushed  :: ConstMap
  }

-- \ The Constant Propogation pass
constProp                             :: Inum Module Module IO a
constProp                              = statefulPass chunk (S "" [] [] [])

chunk                                 :: Iter Module (StateT PassState IO) Module
chunk                                  = dataI >>= mapM toplevelEntity

toplevelEntity                        :: ToplevelEntity -> Iter Module (StateT PassState IO) ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x

stat                                  :: Statement -> Iter Module (StateT PassState IO) Block
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
stat x                                 = return [x]

expr                                  :: Expr -> Iter Module (StateT PassState IO) Expr
expr (ExprVar nm)                      = do
                                           xs <- constMap `fmap` get
                                           return $ maybe (ExprVar nm) ExprConstant (lookup nm xs)
expr (ExprAdd ty e1 e2)                = do
                                           e1' <- expr e1
                                           e2' <- expr e2
                                           case (e1', e2') of
                                             (ExprConstant (LitInteger i1), ExprConstant (LitInteger i2))
                                               -> return $ ExprConstant (LitInteger (i1 + i2))
                                             _ -> return $ ExprAdd ty e1' e2'
expr (ExprPhi ty (x:xs))               = do
                                           x'  <- phiField x
                                           xs' <- mapM phiField xs
                                           return $ if all (== fst x') (map fst xs')
                                                      then fst x'
                                                      else ExprPhi ty (x':xs')
expr e                                 = return e

phiField                              :: (Expr, String) -> Iter Module (StateT PassState IO) (Expr, String)
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

addConst                              :: String -> Literal -> Iter Module (StateT PassState IO) ()
addConst nm x                          = modify $ \st -> st{
                                           constMap   = (nm, x) : constMap st
                                         , notFlushed = (nm, x) : notFlushed st
                                         }

