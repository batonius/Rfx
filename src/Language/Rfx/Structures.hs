module Language.Rfx.Structures(Program(..), Thread(..),
                               ThreadState(..), Oper(..),
                               Expr(..), Var(..),
                               Statment(..),
                               VarType(..),
                               getVarType)
where
import Language.Rfx.Util
import qualified Data.Map as Map
import qualified Data.Set as Set

data Program = Program
    {
      programThreads :: [Thread]
    , programVars :: Set.Set Var
    }
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

data Oper = PlusOp
          | MinusOp
          | MulOp
          | DivOp
          | EualityOp
            deriving (Show, Eq, Ord)

data Expr = NumExpr Int
          | OpExpr Oper Expr Expr
          | VarExpr Var
          | SubExpr Expr
            deriving (Show, Eq, Ord)

data VarType = Int8Type
             | BoolType
               deriving (Show, Eq, Ord)

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
    } deriving (Show, Eq, Ord)

data Statment = AssignSt Var Expr
              | BlockSt [Statment]
              | IfSt Expr Statment Statment
                deriving (Show, Eq)
