{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Block where

import Data.String
  ( fromString
  , IsString
  )
import Data.Monoid
  ( (<>)
  , mempty
  , Monoid
  )

type Module = [ToplevelEntity]

data ToplevelEntity = Function String String [String] [String] Block
                 | Target String String
  deriving (Show, Eq)

type Block = [Statement]

data Statement = Declaration Type String
               | Assignment String Expr
               | Return String Expr
               | Label String
               | Branch String
               | BranchCond String String String
               | Flush
  deriving (Show, Eq)

data Type = TyInteger
          | TyString
          | TyDynamic
  deriving (Show, Eq)

data Expr   = ExprConstant Literal
            | ExprVar String
            | ExprAdd String Expr Expr
  deriving (Show, Eq)

data Literal = LitString String
             | LitInteger Integer
  deriving (Show, Eq)

class Pretty a where
    pretty :: (Monoid s, IsString s) => a -> s

instance Pretty Statement where
    pretty (Declaration _ty s) = "var " <> fromString s
    pretty (Assignment s e)    = fromString s <> " = " <> pretty e
    pretty (Return s e)        = "return " <> fromString s <> " " <> pretty e
    pretty (Label s)           = fromString s <> ":"
    pretty (Branch s)          = "br " <> "%" <> fromString s
    pretty (BranchCond b t f)  = "br " <> "%" <> fromString b <> " " <> "label " <> fromString t <> ", label " <> fromString f
    pretty (Flush)             = mempty

instance Pretty Expr where
    pretty (ExprConstant lit)  = pretty lit
    pretty (ExprVar nm)        = fromString nm
    pretty (ExprAdd ty e1 e2)  = "add " <> fromString ty <> ", " <> pretty e1 <> ", " <> pretty e2

instance Pretty Literal where
    pretty (LitString s)       = fromString (show s)
    pretty (LitInteger x)      = fromString (show x)

