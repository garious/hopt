{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data for the ConstProp pass

module Transforms.ConstPropData where

import Control.Lens.TH
  ( makeLenses
  )
import LlvmData
  ( Literal
  )

-- | A lookup table from a constant's name to its literal value.
type ConstMap = [(String, Literal)]

-- | The State for the ConstProp pass
data PassState = S {
    _currLabel   :: String
  , _constMap    :: ConstMap
  , _constMapMap :: [(String, ConstMap)]
  , _notFlushed  :: ConstMap
  }

makeLenses ''PassState

