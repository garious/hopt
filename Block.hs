{-# LANGUAGE Safe #-}

module Block where

type Module = [ToplevelEntity]

data ToplevelEntity = Function String String [String] [String] Block
                 | Target String String
  deriving (Show, Eq)

type Block = [Statement]

data Statement = Assignment String Expr
               | Return String Expr
               | Label String
               | Branch String
               | BranchCond Expr String String
               | Flush
  deriving (Show, Eq)

data Type = TyInteger
          | TyString
          | TyDynamic
  deriving (Show, Eq)

data Expr   = ExprConstant Literal
            | ExprVar String
            | ExprAdd String Expr Expr
            | ExprPhi String [(Expr, String)]
  deriving (Show, Eq)

data Literal = LitString String
             | LitInteger Integer
             | LitBool Bool
  deriving (Show, Eq)

