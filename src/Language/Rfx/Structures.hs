{-# LANGUAGE TypeFamilies,FlexibleContexts,NamedFieldPuns #-}
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
                               Func(..),
                               FuncName(..),
                               funcName,
                               buildinFunc,
                               funcRetType,
                               funcArgsTypes,
                               opTypes,
                               semOpTypes,
                               posChildOf,
                               getVarType,
                               buildinFuncs)
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
             | FunSynExpr FuncName [SynExpr]
             | StringSynExpr String
             | BoolSynExpr Bool
             | TimeSynExpr Integer -- ms 
               deriving (Show, Eq, Ord)

data SemExpr = NumSemExpr Int
             | OpSemExpr SemOper SemExpr SemExpr
             | VarSemExpr (Var SemExpr)
             | SubSemExpr SemExpr
             | FunSemExpr (Func SemExpr) [SemExpr]
             | StringSemExpr String
             | BoolSemExpr Bool
             | TimeSemExpr Integer -- ms
             deriving (Show, Eq, Ord)
          
data (Expression e) => ProgramPos e = InGlobal
                                   | InThread (Thread e)
                                   | InState (Thread e) (ThreadState e)
                                     deriving (Show, Eq, Ord)
                               
data (Expression e) => Program e = Program
    {
      programThreads :: [Thread e]
    , programVars :: [Var e]
    , programFuncs :: [Func e]
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
             | NEqlSynOp
             | GrSynOp
             | LsSynOp
             | GrEqSynOp
             | LsEqSynOp
             | AndSynOp
             | OrSynOp
             | XorSynOp
            deriving (Show, Eq, Ord)

data SemOper = NumPlusSemOp
             | NumMinusSemOp
             | NumMulSemOp
             | NumEqlSemOp
             | NumNEqlSemOp
             | NumGrSemOp
             | NumLsSemOp
             | NumGrEqlSemOp
             | NumLsEqlSemOp
             | NumDivSemOp
             | BoolAndSemOp
             | BoolOrSemOp
             | BoolXorSemOp
             | TimePlusSemOp
             | TimeMinusSemOp
             | TimeEqlSemOp
             | TimeNEqlSemOp
             | TimeGrSemOp
             | TimeLsSemOp
             | TimeGrEqlSemOp
             | TimeLsEqlSemOp
               deriving (Show, Eq, Ord)
                     
data VarType = Int8Type
             | BoolType
             | StringType
             | TimeType
             | AnyType
               deriving (Show, Eq, Ord)

data VarTypeName = VarTypeName String deriving (Eq, Show, Ord)


data (Expression e) => Func e = BuildinFunc
    {
      biFuncName     ::String
    , biFuncArgs     ::[VarType]
    , biFuncRetType  ::VarType
    }
                             | UserFunc
    {
      uFuncName      ::String
    , uFuncArgs      ::[Var e]
    , uFuncStatments ::[Statment e]
    , uFuncRetType   ::VarType
    }
                               deriving (Show, Eq, Ord)

data FuncName = FuncName String SourcePos deriving (Show, Eq, Ord)
                                        
funcName BuildinFunc{biFuncName} = biFuncName
funcName UserFunc{uFuncName} = uFuncName

buildinFunc BuildinFunc{} = True
buildinFunc _ = False

funcRetType BuildinFunc{biFuncRetType} = biFuncRetType
funcRetType UserFunc{uFuncRetType} = uFuncRetType

funcArgsTypes BuildinFunc{biFuncArgs} = biFuncArgs
funcArgsTypes UserFunc{uFuncArgs} = map varType uFuncArgs

data (Expression e) =>  Statment e = AssignSt (Variable e) e
    | IfSt e [Statment e]
    | IfElseSt e [Statment e] [Statment e]
    | WhileSt e [Statment e]
    | NextSt (ThreadState e) SourcePos
    | BreakSt
    | FunSt e -- Expr == FunExpr
    | ReturnSt e -- TODO support
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
    constExpr :: e -> Bool
    opExpr :: e -> Bool
    subExpr :: e -> Bool
    varExpr :: e -> Bool

instance Expression SynExpr where
    type Variable SynExpr = VarName
    type VariableType SynExpr = VarTypeName
    constExpr NumSynExpr{} = True
    constExpr StringSynExpr{} = True
    constExpr BoolSynExpr{} = True
    constExpr TimeSynExpr{} = True
    constExpr _ = False
    opExpr OpSynExpr{} = True
    opExpr _ = False
    subExpr SubSynExpr{} = True
    subExpr _ = False
    varExpr VarSynExpr{} = True
    varExpr _ = False
                              
instance Expression SemExpr where
    type Variable SemExpr = Var SemExpr
    type VariableType SemExpr = VarType
    constExpr NumSemExpr{} = True
    constExpr StringSemExpr{} = True
    constExpr BoolSemExpr{} = True
    constExpr TimeSemExpr{} = True
    constExpr _ = False
    opExpr OpSemExpr{} = True
    opExpr _ = False
    subExpr SubSemExpr{} = True
    subExpr _ = False
    varExpr VarSemExpr{} = True
    varExpr _ = False

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
               , ("string", StringType)
               , ("time", TimeType)
               , ("ЦЕЛ8", Int8Type)
               , ("ЛОГ", BoolType)
               , ("СТРОКА", StringType)
               , ("ВРЕМЯ", TimeType)]

opTypes :: Map.Map SynOper [SemOper]
opTypes = Map.fromList [(PlusSynOp, [NumPlusSemOp, TimePlusSemOp])
                       ,(MinusSynOp, [NumMinusSemOp, TimeMinusSemOp])
                       ,(MulSynOp, [NumMulSemOp])
                       ,(DivSynOp, [NumDivSemOp])
                       ,(EqlSynOp, [NumEqlSemOp, TimeEqlSemOp])
                       ,(NEqlSynOp, [NumNEqlSemOp, TimeNEqlSemOp])
                       ,(GrSynOp, [NumGrSemOp, TimeGrSemOp])
                       ,(LsSynOp, [NumLsSemOp, TimeLsSemOp])
                       ,(GrEqSynOp, [NumGrEqlSemOp, TimeGrEqlSemOp])
                       ,(LsEqSynOp, [NumLsEqlSemOp, TimeLsEqlSemOp])
                       ,(AndSynOp, [BoolAndSemOp])
                       ,(OrSynOp, [BoolOrSemOp])
                       ,(XorSynOp, [BoolXorSemOp])]


semOpTypes :: [(SemOper, (VarType, VarType, VarType))]
semOpTypes =[(NumPlusSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumMinusSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumMulSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumEqlSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumLsSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumGrSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumLsEqlSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumGrEqlSemOp, (Int8Type, Int8Type, BoolType))
            ,(NumNEqlSemOp, (Int8Type, Int8Type, Int8Type))
            ,(NumDivSemOp, (Int8Type, Int8Type, Int8Type))
            ,(BoolAndSemOp, (BoolType, BoolType, BoolType))
            ,(BoolOrSemOp, (BoolType, BoolType, BoolType))
            ,(BoolXorSemOp, (BoolType, BoolType, BoolType))
            ,(TimePlusSemOp, (TimeType, TimeType, TimeType))
            ,(TimeMinusSemOp, (TimeType, TimeType, TimeType))
            ,(TimeEqlSemOp, (TimeType, TimeType, BoolType))
            ,(TimeLsSemOp, (TimeType, TimeType, BoolType))
            ,(TimeGrSemOp, (TimeType, TimeType, BoolType))
            ,(TimeLsEqlSemOp, (TimeType, TimeType, BoolType))
            ,(TimeGrEqlSemOp, (TimeType, TimeType, BoolType))
            ,(TimeNEqlSemOp, (TimeType, TimeType, TimeType))
            ]

buildinFuncs :: [Func SemExpr]
buildinFuncs = [BuildinFunc{biFuncName="neg"
                           ,biFuncArgs=[Int8Type]
                           ,biFuncRetType=Int8Type}]