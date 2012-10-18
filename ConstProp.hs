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
    constMap :: [(String, Literal)]
  , flushed  :: [String]
  }

-- \ The Constant Propogation pass
constProp                             :: Inum Block Block IO a
constProp                              = statefulPass chunk (S [] [])

chunk                                 :: Iter Block (StateT PassState IO) Block
chunk                                  = concat `fmap` (dataI >>= mapM stat)

stat                                  :: Statement -> Iter Block (StateT PassState IO) Block
stat (Assignment nm (ExprConstant x))  = do
                                           modify (\st -> st{constMap = (nm, x) : constMap st})
                                           return []
stat e@(Assignment nm (ExprVar vNm))   = do
                                           xs <- constMap `fmap` get
                                           case lookup vNm xs of
                                             Just x  -> stat (Assignment nm (ExprConstant x))
                                             Nothing -> return [e]
stat e@(Return (ExprVar vNm))          = do
                                           xs <- constMap `fmap` get
                                           case lookup vNm xs of
                                             Just x  -> return [Return (ExprConstant x)]
                                             Nothing -> return [e]
stat Flush                             = do
                                           st <- get
                                           put st{flushed = map fst (constMap st)}
                                           return [Assignment nm (ExprConstant lit) | (nm, lit) <- constMap st, nm `notElem` flushed st]
stat x                                 = return [x]

