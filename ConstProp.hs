{-# LANGUAGE Safe #-}

module ConstProp where

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , mkInumAutoM
  , ifeed
  , ipopresid
  , ungetI
  , dataI
  )
import Data.IterIO.Trans
  ( runStateTLI
  , liftI
  )

-- From mtl package
import Control.Monad.State
  ( StateT
  , modify
  , get
  )

import Block

-- \ The Constant Propogation pass
constProp :: Inum Block Block IO a
constProp = mkInumAutoM (loop [])
  where 
    loop st = do
      (t', st') <- liftI (runStateTLI constPropChunk st)
      done <- ifeed t'
      if not done then
        loop st'
      else
        ipopresid >>= ungetI

type StateIO a = StateT a IO
type ConstMap = [(String, Literal)]

constPropChunk :: Iter Block (StateIO ConstMap) Block
constPropChunk = concat `fmap` (dataI >>= mapM constPropStat)

constPropStat :: Statement -> Iter Block (StateIO ConstMap) Block
constPropStat (Assignment nm (ExprConstant x)) = do
   modify ((nm, x) :)
   return []
constPropStat e@(Assignment nm (ExprVar vNm)) = do
   xs <- get
   case lookup vNm xs of
     Just x  -> constPropStat (Assignment nm (ExprConstant x))
     Nothing -> return [e]
constPropStat e@(Return (ExprVar vNm)) = do
   xs <- get
   case lookup vNm xs of
     Just x  -> return [Return (ExprConstant x)]
     Nothing -> return [e]
constPropStat x = return [x]

