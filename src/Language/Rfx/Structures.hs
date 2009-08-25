module Language.Rfx.Structures(Program(..), Thread(..),
                               ThreadState(..), Operator(..),
                               Expr(..), Var(..),
                               Statment(..),
                               VarType(..),
                               getVarType)
where
import Language.Rfx.Util
import qualified Data.Map as Map

data Program = Program [Thread]
               deriving (Show, Eq)

data Thread = Thread
    {
      threadName :: String
    , states :: [ThreadState]
    } deriving (Show, Eq)

data ThreadState = ThreadState
    {
      stateName :: String
    , statments :: [Statment]
    } deriving (Show, Eq)

data Operator = PlusOp
              | MinusOp
              | MulOp
              | EualityOp
                deriving (Show, Eq)

data Expr = NumExpr Int
          | OpExpr Operator Expr Expr
          | VarExpr Var
            deriving (Show, Eq)

data VarType = Int8Type
             | BoolType
               deriving (Show, Eq)

getVarType :: String -> Maybe VarType
getVarType s = Map.lookup s $ Map.fromList $
               [ ("INT8", Int8Type)
               , ("BOOL", BoolType)
               , ("ЦЕЛ8", Int8Type)
               , ("ЛОГ", BoolType)]
                        
data Var = Var
    {
      varName :: String
    , varInitValue :: Expr
    , varScope :: ProgramPos
    , varType :: VarType
    } deriving (Show, Eq)

data Statment = AssignSt Var Expr
              | BlockSt [Statment]
              | IfSt Expr Statment Statment
                deriving (Show, Eq)
