{-# LANGUAGE Safe #-}

module DeadInstructionElimination where

-- | Dead Instruction Elimination
--
--   This optimization pass removes assignment statements that are 
--   unreferenced by all later statements.  Unlike the LLVM version,
--   this version also moves each instruction as closely as possible to
--   the first instruction that references it.

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
  , put
  )
import OptPassUtils
  ( statefulPass
  )
import Block

data PassState = S {
    statements   :: [(String, Expr)]
  , notFlushed :: [(String, Expr)]
  }

-- \ The DeadInstructionElimination pass
deadInstructionElimination            :: Inum Module Module IO a
deadInstructionElimination             = statefulPass chunk (S [] [])

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
                                           xss <- mapM popStatement (vars e)
                                           pushStatement nm e
                                           return $ concat xss
stat s@(Return _ e)                    = do
                                           xss <- mapM popStatement (vars e)
                                           return $ concat xss ++ [s]
stat Flush                             = do
                                           st <- get
                                           put st{notFlushed = []}
                                           return [Assignment nm e | (nm, e) <- notFlushed st]
stat x                                 = return [x]

vars                                  :: Expr -> [String]
vars (ExprVar nm)                      = [nm]
vars (ExprAdd _ty e1 e2)               = vars e1 ++ vars e2
vars (ExprConstant _)                  = []
vars (ExprPhi _ _)                     = []

pushStatement                         :: String -> Expr -> Iter Module (StateT PassState IO) ()
pushStatement nm x                     = modify (\st -> st{
                                           statements = (nm, x) : statements st
                                         , notFlushed = (nm, x) : notFlushed st
                                         })

popStatement                          :: String -> Iter Module (StateT PassState IO) [Statement]
popStatement nm                          = do 
                                             xs <- statements `fmap` get
                                             case lookup nm xs of
                                               Just x -> do
                                                 modify (\st -> st{
                                                   statements = delete (nm,x) xs
                                                 , notFlushed = delete (nm,x) (notFlushed st)
                                                 })
                                                 return [Assignment nm x]
                                               Nothing -> return []

