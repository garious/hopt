{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Block where

import Data.String
  ( fromString
  , IsString
  )
import Data.Monoid
  ( (<>)
  , Monoid
  )

type Block = [Statement]

data Statement = Declaration Type String
               | Assignment String Expr
               | Return Expr
               | Flush
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
    pretty :: (Monoid s, IsString s) => a -> s

instance Pretty Statement where
    pretty (Declaration _ty s) = "var " <> fromString s
    pretty (Assignment s e)    = fromString s <> " = " <> pretty e
    pretty (Return e)          = "return " <> pretty e
    pretty (Flush)             = ""

instance Pretty Expr where
    pretty (ExprConstant lit)  = pretty lit
    pretty (ExprVar nm)        = fromString nm

instance Pretty Literal where
    pretty (LitString s)       = fromString (show s)
    pretty (LitInteger x)      = fromString (show x)

