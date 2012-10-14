{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Block where

import Data.String
  ( fromString
  )
import Data.Monoid
  ( (<>)
  )
import qualified Data.ByteString.Lazy.Char8 as L

type Block = [Statement]

data Statement = Declaration Type String
               | Assignment String Expr
               | Return Expr
  deriving (Show, Eq)

data Type = TyInteger
          | TyString
          | TyDynamic
  deriving (Show, Eq)

data Expr   = ExprConstant Literal
            | ExprVar String
  deriving (Show, Eq)

data Literal = LitString String
             | LitInteger Integer
  deriving (Show, Eq)

class Pretty a where
    pretty :: a -> L.ByteString

instance Pretty Statement where
    pretty (Declaration _ty s) = "var " <> fromString s
    pretty (Assignment s e)    = fromString s <> " = " <> pretty e
    pretty (Return e)          = "return " <> pretty e

instance Pretty Expr where
    pretty (ExprConstant lit)  = pretty lit
    pretty (ExprVar nm)        = fromString nm

instance Pretty Literal where
    pretty (LitString s)       = fromString (show s)
    pretty (LitInteger x)      = fromString (show x)

