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
  )
import OptPassUtils
  ( statefulPass
  )
import Block

type ConstMap = [(String, Literal)]

-- \ The Constant Propogation pass
constProp :: Inum Block Block IO a
constProp = statefulPass chunk []

chunk :: Iter Block (StateT ConstMap IO) Block
chunk = concat `fmap` (dataI >>= mapM stat)

stat :: Statement -> Iter Block (StateT ConstMap IO) Block
stat (Assignment nm (ExprConstant x)) = do
   modify ((nm, x) :)
   return []
stat e@(Assignment nm (ExprVar vNm)) = do
   xs <- get
   case lookup vNm xs of
     Just x  -> stat (Assignment nm (ExprConstant x))
     Nothing -> return [e]
stat e@(Return (ExprVar vNm)) = do
   xs <- get
   case lookup vNm xs of
     Just x  -> return [Return (ExprConstant x)]
     Nothing -> return [e]
stat x = return [x]

