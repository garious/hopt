{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The configuration for Hopt and lenses to access each field.
--   This small module is kept separate from the rest, because 
--   it uses TemplateHaskell to generate the Lens instances.

module HoptArgData where

-- From lens package
import Control.Lens.TH
  ( makeLenses
  )

-- | Configuration data for Hopt
data Cfg = Cfg {
    _optPasses  :: [String]  -- ^ List of optimization passes
  , _inFile     :: FilePath  -- ^ Input file
  , _outFile    :: FilePath  -- ^ Output file
  , _isText     :: Bool      -- ^ Output as text, not bitcode
  } deriving (Show, Eq)

-- Use Template Haskell to generate getters and setters
-- for each field in Cfg
makeLenses ''Cfg

