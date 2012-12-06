{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | LLVM Data types

module LlvmData where

import Control.Lens.Plated
  ( Plated
  , plate
  )
import Data.Data
  ( Data
  , Typeable
  )
import Data.Data.Lens
  ( uniplate
  )

-- | A list of top-level entities such as target declarations or function definitions
type Module                    = [ToplevelEntity]

-- | A top-level entity within a module
data ToplevelEntity            = Function String String [String] [String] Block
                               | Target String String
                                 deriving (Show, Eq)

-- | A block of statements
type Block                     = [Statement]

-- | A statement
data Statement                 = Assignment String Expr
                               | Return String Expr
                               | Label String
                               | Branch String
                               | BranchCond Expr String String
                               | Flush
                                 deriving (Show, Eq)

-- | A type
data Type                      = TyInteger
                               | TyString
                               | TyDynamic
                                 deriving (Show, Eq)

-- | An expression
data Expr                      = ExprConstant Literal
                               | ExprVar String
                               | ExprAdd String Expr Expr
                               | ExprPhi String [(Expr, String)]
                                 deriving (Show, Eq, Data, Typeable)

-- | A literal value
data Literal                   = LitString String
                               | LitInteger Integer
                               | LitBool Bool
                                 deriving (Show, Eq, Data, Typeable)


instance Plated Expr where
   plate = uniplate

