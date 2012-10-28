{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module ToLlvm where

import Data.String
  ( fromString
  , IsString
  )
import Data.List
  ( intercalate
  )
import Data.Monoid
  ( (<>)
  , mempty
  , mconcat
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
    toLlvm :: (Eq s, Monoid s, IsString s) => a -> s

instance ToLlvm ToplevelEntity where
    toLlvm (Function ret nm args as blk)  = "define " <> fromString ret
                                        <+> "@" <> fromString nm
                                         <> "(" <> fromString (intercalate ", " args) <> ")"
                                        <+> fromString (unwords as)
                                        <+> "{\n" <> mconcat (map (bbLine . toLlvm) blk) <> "}"

    toLlvm (Target nm val)                = "target " <> fromString nm <> " = \"" <> fromString val <> "\""


bbLine :: (Monoid s, IsString s) => s -> s
bbLine s = "  " <> s <> "\n"


instance ToLlvm Statement where
    toLlvm (Declaration _t _s) = mempty
    toLlvm (Assignment s e)    = "%" <> fromString s <> " = " <> toLlvm e
    toLlvm (Return s e)        = "ret " <> fromString s <+> toLlvm e
    toLlvm (Label s)           = fromString s <> ":"
    toLlvm (Branch s)          = "br " <> "%" <> fromString s
    toLlvm (BranchCond b t f)  = "br " <> "%" <> fromString b <+> "label " <> fromString t <> ", label " <> fromString f
    toLlvm (Flush)             = mempty

instance ToLlvm Expr where
    toLlvm (ExprConstant lit)  = toLlvm lit
    toLlvm (ExprVar nm)        = "%" <> fromString nm
    toLlvm (ExprAdd ty e1 e2)  = "add " <> fromString ty <> ", " <> toLlvm e1 <> ", " <> toLlvm e2

instance ToLlvm Literal where
    toLlvm (LitString s)       = fromString (show s)
    toLlvm (LitInteger x)      = fromString (show x)

-- Concat with a space between, unless one is empty
(<+>) :: (Eq s, Monoid s, IsString s) => s -> s -> s
a <+> b
  | a == mempty = b
  | b == mempty = a
  | otherwise   = a <> " " <> b

printFlow :: Inum Module L.ByteString IO a
printFlow = mkInum $ (L.unlines . map toLlvm) `fmap` dataI
