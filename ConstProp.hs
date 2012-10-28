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

data PassState = S {
    constMap   :: [(String, Literal)]
  , notFlushed :: [(String, Literal)]
  }

-- \ The Constant Propogation pass
constProp                             :: Inum Module Module IO a
constProp                              = statefulPass chunk (S [] [])

chunk                                 :: Iter Module (StateT PassState IO) Module
chunk                                  = dataI >>= mapM toplevelEntity

toplevelEntity                        :: ToplevelEntity -> Iter Module (StateT PassState IO) ToplevelEntity
toplevelEntity (Function nm ret args as blk)
                                       = do
                                           blk' <- mapM stat blk
                                           return $ Function nm ret args as (concat blk')
toplevelEntity x                       = return x

stat                                  :: Statement -> Iter Module (StateT PassState IO) Block
stat (Assignment nm (ExprConstant x))  = do
                                           addConst nm x
                                           return []
stat e@(Assignment nm (ExprVar vNm))   = do
                                           xs <- constMap `fmap` get
                                           case lookup vNm xs of
                                             Just x  -> stat (Assignment nm (ExprConstant x))
                                             Nothing -> return [e]
stat e@(Return s (ExprVar vNm))        = do
                                           xs <- constMap `fmap` get
                                           case lookup vNm xs of
                                             Just x  -> return [Return s (ExprConstant x)]
                                             Nothing -> return [e]
stat Flush                             = do
                                           st <- get
                                           put st{notFlushed = []}
                                           return [Assignment nm (ExprConstant lit) | (nm, lit) <- notFlushed st]
stat x                                 = return [x]

addConst                              :: String -> Literal -> Iter Module (StateT PassState IO) ()
addConst nm x                          = modify (\st -> st{
                                           constMap   = (nm, x) : constMap st
                                         , notFlushed = (nm, x) : notFlushed st
                                         })

