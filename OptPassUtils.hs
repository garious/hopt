{-# LANGUAGE Safe #-}

module OptPassUtils where

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , ChunkData
  , mkInumAutoM
  , ifeed
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

statefulPass :: (Monad m, ChunkData tOut) => Iter tOut (StateT s m) tOut -> s -> Inum tOut tOut m a
statefulPass iter s = mkInumAutoM (loop s)
  where 
    loop st = do
      (t', st') <- liftI (runStateTLI iter st)
      done <- ifeed t'
      if not done
        then loop st'
        else ipopresid >>= ungetI

