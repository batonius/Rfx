{-# LANGUAGE TypeFamilies #-}
module Language.Rfx.Structures(Program(..),
                               Thread(..),
                               ThreadState(..),
                               SynOper(..),
                               SemOper(..),
                               SynExpr(..),
                               SemExpr(..),
                               Var(..),
                               Statment(..),
                               VarType(..),
                               ProgramPos(..),
                               VarName(..),
                               posChildOf,
                               getVarType)
where
import Language.Rfx.Util
import qualified Data.Map as Map
import qualified Data.Set as Set

data (Expression e) => Var e = Var
    {
      varName :: String
    , varInitValue :: e
    , varScope :: ProgramPos e
    , varType :: VarType
    } deriving (Show, Ord)

instance (Expression e) => Eq (Var e) where
    (==) a b = (varName a) == (varName b)
         
class (Eq e) => Expression e where
    data Variable e :: *

data SynExpr = NumSynExpr Int
          | OpSynExpr SynOper SynExpr SynExpr
          | VarSynExpr VarName
          | SubSynExpr SynExpr
          | FunSynExpr String [SynExpr]
          | StringSynExpr String
            deriving (Show, Eq, Ord)

instance Expression SynExpr where
    data Variable SynExpr = VarName String
                          | LongVarName String String
                            deriving (Eq, Ord, Show)

type VarName = Variable SynExpr
                               
data SemExpr = NumSemExpr Int
             | OpSemExpr SemOper SemExpr SemExpr
             | VarSemExpr (Var SemExpr)
             | SubSemExpr SemExpr
             | FunSemExpr ()
             | StringSemExpr String
             deriving (Show, Eq, Ord)
                      
instance Expression SemExpr where
    data Variable SemExpr = SemVar (Var SemExpr)
                            deriving (Eq, Ord, Show)
          
data (Expression e) => ProgramPos e = InGlobal
                                   | InThread (Thread e)
                                   | InState (Thread e) (ThreadState e)
                                     deriving (Show, Eq, Ord)

posChildOf :: (Expression e) => ProgramPos e -> ProgramPos e -> Bool
posChildOf a b = case b of
                 InGlobal -> True
                 InThread btn -> case a of
                                  InThread atn -> atn == btn
                                  InState atn _ -> atn == btn
                                  InGlobal -> False
                 InState _ _ -> a == b
                               
data (Expression e) => Program e = Program
    {
      programThreads :: [Thread e]
    , programVars :: [Var e]
    } deriving (Show, Eq)

data (Expression e) => Thread e = Thread
    {
      threadName :: String
    , threadStates :: [ThreadState e]
    } deriving (Show, Eq, Ord)

data (Expression e) => ThreadState e = ThreadState
    {
      stateName :: String
    , stateStatments :: [Statment e]
    } deriving (Show, Eq, Ord)

data SynOper = PlusSynOp
          | MinusSynOp
          | MulSynOp
          | DivSynOp
          | EqualitySynOp
          | GrSynOp
          | LsSynOp
          | GrEqSynOp
          | LsEqSynOp
            deriving (Show, Eq, Ord)

data SemOper = NumPlusSynOp
             | TimePlusSynOp
             | StringPlusSynOp
             | NumMinuxSynOp
               deriving (Show, Eq, Ord)
                     
data VarType = Int8Type
             | BoolType
             | StringType
               deriving (Show, Eq, Ord)

getVarType :: String -> Maybe VarType
getVarType s = Map.lookup s $ Map.fromList $
               [ ("int8", Int8Type)
               , ("bool", BoolType)
               , ("ЦЕЛ8", Int8Type)
               , ("ЛОГ", BoolType)
               , ("string", StringType)
               , ("СТРОКА", StringType)]

data (Expression e) =>  Statment e = AssignSt (Variable e) e
    | IfSt e [Statment e]
    | IfElseSt e [Statment e] [Statment e]
    | WhileSt e [Statment e]
    | NextSt (ThreadState e)
    | BreakSt
    | FunSt e -- Expr == FunExpr
      deriving (Show, Eq, Ord)
