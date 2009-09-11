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
    , threadStates :: [ThreadState]
    } deriving (Show, Eq)

data ThreadState = ThreadState
    {
      stateName :: String
    , stateStatments :: [Statment]
    } deriving (Show, Eq)

data Oper = PlusOp
          | MinusOp
          | MulOp
          | DivOp
          | EqualityOp
          | GrOp
          | LsOp
          | GrEqOp
          | LsEqOp
            deriving (Show, Eq, Ord)

data Expr = NumExpr Int
          | OpExpr Oper Expr Expr
          | VarExpr Var
          | SubExpr Expr
          | FunExpr String [Expr]
            deriving (Show, Eq, Ord)

data VarType = Int8Type
             | BoolType
             | CheckMeType -- Set it to get thread variable at compile time
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
              | IfSt Expr [Statment]
              | IfElseSt Expr [Statment] [Statment]
              | WhileSt Expr [Statment]
              | NextSt ThreadState
              | BreakSt
              | FunSt Expr -- Expr == FunExpr
                deriving (Show, Eq)
