{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module ToLlvm where

import Data.String
  ( fromString
  , IsString
  )
import Data.Monoid
  ( (<>)
  , mempty
  , Monoid
  )

import Block

class ToLlvm a where
    toLlvm :: (Monoid s, IsString s) => a -> s

instance ToLlvm Statement where
    toLlvm (Declaration _t _s) = mempty
    toLlvm (Assignment s e)    = "%" <> fromString s <> " = " <> toLlvm e
    toLlvm (Return e)          = "ret " <> toLlvm e
    toLlvm (Flush)             = mempty

instance ToLlvm Expr where
    toLlvm (ExprConstant lit)  = toLlvm lit
    toLlvm (ExprVar nm)        = "%" <> fromString nm

instance ToLlvm Literal where
    toLlvm (LitString s)       = fromString (show s)
    toLlvm (LitInteger x)      = fromString (show x)
