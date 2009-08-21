module Language.Rfx.Structures(Program(..), Thread(..),
                               State(..), Operator(..),
                               Expr(..), Variable(..),
                               Statment(..))
where
  
data Program = Program [Thread]
               deriving (Show, Eq)
                        
data Thread = Thread [State]
              deriving (Show, Eq)
                       
data State = State [Statment]
             deriving (Show, Eq)

data Operator = PlusOp
              | MinusOp
              | MulOp
                deriving (Show, Eq)
                      
data Expr = NumExpr Int
          | OpExp Operator Expr Expr
          | VarOp Variable
            deriving (Show, Eq)

data Variable = Variable String
                deriving (Show, Eq)
            
data Statment = Assign Variable Expr
              | Block [Statment]
              | If Expr Statment Statment
                deriving (Show, Eq)
                