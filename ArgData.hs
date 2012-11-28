{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module ArgData where

-- From lens package
import Control.Lens.TH
  ( makeLenses
  )

data Cfg = Cfg {
    _optPasses  :: [String]
  , _inFile     :: FilePath
  , _outFile    :: FilePath
  , _isText     :: Bool      -- Output as text, not bitcode
  } deriving (Show, Eq)

-- Use Template Haskell to generate getters and setters
-- for each field in Cfg
makeLenses ''Cfg

