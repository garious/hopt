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
import Data.IterIO
  ( Inum
  , mkInum
  , dataI
  )
import qualified Data.ByteString.Lazy.Char8 as L

import Block

class ToLlvm a where
    toLlvm :: (Monoid s, IsString s) => a -> s

instance ToLlvm Statement where
    toLlvm (Declaration _t _s) = mempty
    toLlvm (Assignment s e)    = "%" <> fromString s <> " = " <> toLlvm e
    toLlvm (Return s e)        = "ret " <> fromString s <> " " <> toLlvm e
    toLlvm (Label s)           = fromString s <> ":"
    toLlvm (Branch s)          = "br " <> "%" <> fromString s
    toLlvm (BranchCond b t f)  = "br " <> "%" <> fromString b <> " " <> "label " <> fromString t <> ", label " <> fromString f
    toLlvm (Flush)             = mempty

instance ToLlvm Expr where
    toLlvm (ExprConstant lit)  = toLlvm lit
    toLlvm (ExprVar nm)        = "%" <> fromString nm
    toLlvm (ExprAdd ty e1 e2)  = "add " <> fromString ty <> ", " <> toLlvm e1 <> ", " <> toLlvm e2

instance ToLlvm Literal where
    toLlvm (LitString s)       = fromString (show s)
    toLlvm (LitInteger x)      = fromString (show x)

printFlow :: Inum Block L.ByteString IO a
printFlow = mkInum $ (L.unlines . map toLlvm) `fmap` dataI
