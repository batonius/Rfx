{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module Language.Rfx.Structures(Program(..),
                               Thread(..),
                               ThreadState(..),
                               SynOper(..),
                               SemOper(..),
                               SynExpr(..),
                               SemExpr(..),
                               Var(..),
                               Expression(..),
                               Statment(..),
                               VarType(..),
                               ProgramPos(..),
                               VarName(..),
                               VarTypeName(..),
                               opTypes,
                               semOpTypes,
                               posChildOf,
                               getVarType)
where
import Language.Rfx.Util
import Text.ParserCombinators.Parsec(SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set

data (Expression e) => Var e = Var
    {
      varName :: String
    , varInitValue :: e
    , varScope :: ProgramPos e
    , varType :: (VariableType e)
    , varSourcePos :: SourcePos
    } deriving (Show, Ord)
                            
instance (Expression e) => Eq (Var e) where
    (==) a b = (varName a) == (varName b)

data VarName = VarName String SourcePos
             | LongVarName String String SourcePos
               deriving (Eq, Ord, Show)
               
data SynExpr = NumSynExpr Int
             | OpSynExpr SynOper SynExpr SynExpr SourcePos
             | VarSynExpr VarName 
             | SubSynExpr SynExpr
             | FunSynExpr String [SynExpr]
             | StringSynExpr String
               deriving (Show, Eq, Ord)

data SemExpr = NumSemExpr Int
             | OpSemExpr SemOper SemExpr SemExpr
             | VarSemExpr (Var SemExpr)
             | SubSemExpr SemExpr
             | FunSemExpr ()
             | StringSemExpr String
             deriving (Show, Eq, Ord)
          
data (Expression e) => ProgramPos e = InGlobal
                                   | InThread (Thread e)
                                   | InState (Thread e) (ThreadState e)
                                     deriving (Show, Eq, Ord)
                               
data (Expression e) => Program e = Program
    {
      programThreads :: [Thread e]
    , programVars :: [Var e]
    } deriving (Show, Eq)

data (Expression e) => Thread e = Thread
    {
      threadName :: String
    , threadStates :: [ThreadState e]
    } deriving (Show, Ord)

instance Expression e => Eq (Thread e) where
    (==) a b = (threadName a) == (threadName b)
                               
data (Expression e) => ThreadState e = ThreadState
    {
      stateName :: String
    , stateStatments :: [Statment e]
    } deriving (Show, Ord)

instance Expression e => Eq (ThreadState e) where
    (==) a b = (stateName a) == (stateName b)
                                    
data SynOper = PlusSynOp
          | MinusSynOp
          | MulSynOp
          | DivSynOp
          | EqlSynOp
          | GrSynOp
          | LsSynOp
          | GrEqSynOp
          | LsEqSynOp
            deriving (Show, Eq, Ord)

data SemOper = NumPlusSemOp
             | NumMinusSemOp
             | NumMulSemOp
             | TimePlusSemOp
             | StringPlusSemOp
             | NumMinuxSemOp
             | NumEqlSemOp
             | NumNEqlSemOp
             | NumGrSemOp
             | NumLsSemOp
             | NumDivSemOp
               deriving (Show, Eq, Ord)
                     
data VarType = Int8Type
             | BoolType
             | StringType
             | DateTimeType
             | AnyType
               deriving (Show, Eq, Ord)

data VarTypeName = VarTypeName String deriving (Eq, Show, Ord)
                        
data (Expression e) =>  Statment e = AssignSt (Variable e) e
    | IfSt e [Statment e]
    | IfElseSt e [Statment e] [Statment e]
    | WhileSt e [Statment e]
    | NextSt (ThreadState e)
    | BreakSt
    | FunSt e -- Expr == FunExpr
      deriving (Show, Eq, Ord)
               
class (Eq e
      ,Eq (Variable e)
      ,Ord (Variable e)
      ,Show (Variable e)
      ,Eq (VariableType e)
      ,Ord (VariableType e)
      ,Show (VariableType e)) => Expression e where
    type Variable e :: *
    type VariableType e :: *

instance Expression SynExpr where
    type Variable SynExpr = VarName
    type VariableType SynExpr = VarTypeName
                              
instance Expression SemExpr where
    type Variable SemExpr = Var SemExpr
    type VariableType SemExpr = VarType

posChildOf :: (Expression e) => ProgramPos e -> ProgramPos e -> Bool
posChildOf a b = case b of
                 InGlobal -> True
                 InThread btn -> case a of
                                  InThread atn -> atn == btn
                                  InState atn _ -> atn == btn
                                  InGlobal -> False
                 InState _ _ -> a == b
                               
getVarType :: String -> Maybe VarType
getVarType s = Map.lookup s $ Map.fromList $
               [ ("int8", Int8Type)
               , ("bool", BoolType)
               , ("ЦЕЛ8", Int8Type)
               , ("ЛОГ", BoolType)
               , ("string", StringType)
               , ("СТРОКА", StringType)]

opTypes :: Map.Map SynOper [SemOper]
opTypes = Map.fromList [(PlusSynOp, [NumPlusSemOp])
                       ,(MinusSynOp, [NumMinusSemOp])
                       ,(MulSynOp, [NumMulSemOp])
                       ,(DivSynOp, [NumDivSemOp])
                       ,(EqlSynOp, [NumEqlSemOp])
                       ,(GrSynOp, [NumGrSemOp])
                       ,(LsSynOp, [NumLsSemOp])
                       ,(GrEqSynOp, [])
                       ,(LsEqSynOp, [])]


semOpTypes :: [(SemOper, (VarType, VarType, VarType))]
semOpTypes =[(NumPlusSemOp, (Int8Type, Int8Type, Int8Type))
            ,(StringPlusSemOp, (StringType, StringType, StringType))
            ,(NumMinusSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumMulSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumEqlSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumLsSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumGrSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumNEqlSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumDivSemOp, (Int8Type, Int8Type, Int8Type))]
                                  