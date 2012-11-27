{-# LANGUAGE Safe #-}

module OptPassUtils where

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , ChunkData
  , mkInumAutoM
  , ifeed
  , dataI
  , ipopresid
  , ungetI
  )
import Data.IterIO.Trans
  ( runStateTLI
  , liftI
  )
import Control.Monad.State
  ( StateT
  )

statefulPass :: (Monad m, ChunkData tOut) => (tOut -> Iter tOut (StateT s m) tOut) -> s -> Inum tOut tOut m a
statefulPass iter = mkInumAutoM . loop
  where 
    loop st = do
      x <- dataI
      (t', st') <- liftI $ runStateTLI (iter x) st
      done <- ifeed t'
      if not done
        then loop st'
        else ipopresid >>= ungetI

