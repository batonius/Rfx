module Language.Rfx.Structures(Program(..), Thread(..),
                               ThreadState(..), Operator(..),
                               Expr(..), Variable(..),
                               Statment(..))
where

data Program = Program [Thread]
               deriving (Show, Eq)

data Thread = Thread
    {
        threadName :: String
      , states :: [ThreadState]
    }
              deriving (Show, Eq)

data ThreadState = ThreadState
    {
        stateName :: String
      , statments :: [Statment]
    }
             deriving (Show, Eq)

data Operator = PlusOp
              | MinusOp
              | MulOp
              | EualityOp
                deriving (Show, Eq)

data Expr = NumExpr Int
          | OpExp Operator Expr Expr
          | VarOp Variable
            deriving (Show, Eq)

data Variable = Variable String
                deriving (Show, Eq)

data Statment = AssignSt Variable Expr
              | BlockSt [Statment]
              | IfSt Expr Statment Statment
                deriving (Show, Eq)
