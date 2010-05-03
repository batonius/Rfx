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
                               ThreadName(..),
                               StateName(..),
                               RValue(..),
                               funcName,
                               buildinFunc,
                               funcRetType,
                               funcArgsTypes,
                               opTypes,
                               semOpTypes,
                               posChildOf,
                               buildinFuncs,
                               statmentPos,
                               typeCanBe,
                               typeOfExpr,
                               isArrayType)
where
import Text.ParserCombinators.Parsec(SourcePos)
import qualified Data.Map as Map

-- Program
data (Expression e) => Program e = Program
    {
      programThreads :: [Thread e]
    , programVars :: [Var e]
    , programFuncs :: [Func e]
    } deriving (Show, Eq)
                                
-- Real objects
data (Expression e) => Thread e = Thread
    {
      threadName :: String
    , threadStates :: [ThreadState e]
    } deriving (Show, Ord)
                               
data (Expression e) => ThreadState e = ThreadState
    {
      stateName :: String
    , stateStatments :: [Statment e]
    } deriving (Show, Ord)

data (Expression e) => Var e = Var
    {
      varName :: String
    , varInitValue :: e
    , varScope :: ProgramPos e
    , varType :: (EVariableType e)
    , varSourcePos :: SourcePos
    , varArg :: Bool
    }
                              deriving (Show, Ord)

data VarType = Int8Type
             | Int16Type
             | Int32Type
             | BoolType
             | StringType
             | TimeType
             | VoidType
             | ArrayType
               {
                 arrayType :: VarType
                 ,arraySize :: Integer
               }
               deriving (Show, Eq, Ord)

data (Expression e) => RValue e = RValueVar (EVariable e)
                               | RValueArrayAccess (RValue e) e
                               deriving (Show, Eq, Ord)
                        
data (Expression e) => Func e = BuildinFunc
    {
      biFuncName       ::String
    , biFuncTargetName :: String
    , biFuncArgs       ::[VarType]
    , biFuncRetType    ::(EVariableType e)
    }
                             | UserFunc
    {
      uFuncName      ::String
    , uFuncArgs      ::[Var e]
    , uFuncStatments ::[Statment e]
    , uFuncRetType   ::(EVariableType e)
    , uFuncPos       ::SourcePos
    }
                               deriving (Show, Ord)
-- Names
data ThreadName = ThreadName String deriving (Show, Eq, Ord)
data StateName = StateName String deriving (Show, Eq, Ord)
data VarName = VarName String
             | LongVarName String String
               deriving (Eq, Ord, Show)
data VarTypeName = VarTypeName String
                 | ArrayVarTypeName VarTypeName Integer
                   deriving (Eq, Show, Ord)
data FuncName = FuncName String  deriving (Show, Eq, Ord)
              
-- Expr class
class (Eq e
      ,Eq (EVariable e)
      ,Ord (EVariable e)
      ,Show (EVariable e)
      ,Eq (EVariableType e)
      ,Ord (EVariableType e)
      ,Show (EVariableType e)
      ,Eq (EFunction e)
      ,Ord (EFunction e)
      ,Show (EFunction e)
      ,Eq (EThread e)
      ,Ord (EThread e)
      ,Show (EThread e)
      ,Eq (EState e)
      ,Ord (EState e)
      ,Show (EState e)
      ) => Expression e where
    type EVariable e :: *
    type EVariableType e :: *
    type EFunction e :: *
    type EThread e :: *
    type EState e :: *
    constExpr :: e -> Bool
    opExpr :: e -> Bool
    subExpr :: e -> Bool
    varExpr :: e -> Bool
    voidExpr :: e -> Bool

-- Expr instances                
data SynExpr = NumSynExpr Integer SourcePos
             | OpSynExpr SynOper SynExpr SynExpr SourcePos 
             | VarSynExpr (EVariable SynExpr) SourcePos
             | SubSynExpr SynExpr
             | FunSynExpr (EFunction SynExpr) [SynExpr] SourcePos
             | StringSynExpr String
             | BoolSynExpr Bool
             | TimeSynExpr Integer -- ms
             | VoidSynExpr
             | ArrayAccessSynExpr SourcePos SynExpr SynExpr 
               deriving (Show, Eq, Ord)

instance Expression SynExpr where
    type EVariable SynExpr = VarName
    type EVariableType SynExpr = VarTypeName
    type EFunction SynExpr = FuncName
    type EThread SynExpr = ThreadName
    type EState SynExpr = StateName
    constExpr NumSynExpr{} = True
    constExpr StringSynExpr{} = True
    constExpr BoolSynExpr{} = True
    constExpr TimeSynExpr{} = True
    constExpr VoidSynExpr = True
    constExpr _ = False
    opExpr OpSynExpr{} = True
    opExpr _ = False
    subExpr SubSynExpr{} = True
    subExpr _ = False
    varExpr VarSynExpr{} = True
    varExpr _ = False
    voidExpr VoidSynExpr = True
    voidExpr _ = False

data SemExpr = NumSemExpr Integer
             | OpSemExpr SemOper SemExpr SemExpr VarType
             | VarSemExpr (EVariable SemExpr)
             | SubSemExpr SemExpr
             | FunSemExpr (EFunction SemExpr) [SemExpr]
             | StringSemExpr String
             | BoolSemExpr Bool
             | TimeSemExpr Integer -- ms
             | VoidSemExpr
             | ArrayAccessSemExpr SemExpr SemExpr
             deriving (Show, Eq, Ord)
                              
instance Expression SemExpr where
    type EVariable SemExpr = Var SemExpr
    type EVariableType SemExpr = VarType
    type EFunction SemExpr = Func SemExpr
    type EThread SemExpr = Thread SemExpr
    type EState SemExpr = ThreadState SemExpr
    constExpr NumSemExpr{} = True
    constExpr StringSemExpr{} = True
    constExpr BoolSemExpr{} = True
    constExpr TimeSemExpr{} = True
    constExpr VoidSemExpr = True
    constExpr _ = False
    opExpr OpSemExpr{} = True
    opExpr _ = False
    subExpr SubSemExpr{} = True
    subExpr _ = False
    varExpr VarSemExpr{} = True
    varExpr _ = False
    voidExpr VoidSemExpr = True
    voidExpr _ = False

                      
-- Eq instances                      
instance (Expression e) => Eq (Var e) where
    (==) a b = (varName a) == (varName b)

instance Expression e => Eq (Thread e) where
    (==) a b = (threadName a) == (threadName b)
                               
instance Expression e => Eq (ThreadState e) where
    (==) a b = (stateName a) == (stateName b)
               
instance (Expression e) => Eq (Func e) where
    (==) BuildinFunc{biFuncName=biFuncNamel} BuildinFunc{biFuncName=biFuncNamer} = biFuncNamel == biFuncNamer
    (==) UserFunc{uFuncName=uFuncNamel} UserFunc{uFuncName=uFuncNamer} = uFuncNamel == uFuncNamer
    (==) _ _ = False

-- ProgramPos               
data (Expression e) => ProgramPos e = InGlobal
                                   | InThread (EThread e)
                                   | InState (EThread e) (EState e)
                                   | InFunction (EFunction e)
                                     deriving (Show, Eq, Ord)

-- Operators                                              
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
             | BoolEqlSemOp
             | BoolNEqlSemOp
             | TimePlusSemOp
             | TimeMinusSemOp
             | TimeEqlSemOp
             | TimeNEqlSemOp
             | TimeGrSemOp
             | TimeLsSemOp
             | TimeGrEqlSemOp
             | TimeLsEqlSemOp
               deriving (Show, Eq, Ord)

-- StatEments, lol
data (Expression e) => Statment e = AssignSt (RValue e) e SourcePos
                                 | IfSt e [Statment e] SourcePos
                                 | IfElseSt e [Statment e] [Statment e] SourcePos
                                 | WhileSt e [Statment e] SourcePos 
                                 | NextSt (EState e) SourcePos
                                 | BreakSt SourcePos
                                 | FunSt e SourcePos
                                 | ReturnSt e SourcePos
                                 | WaitSt e Int SourcePos
                                   deriving (Show, Eq, Ord)

-- Functions
statmentPos :: (Expression e) => Statment e -> SourcePos
statmentPos (AssignSt _ _ pos) = pos
statmentPos (IfSt _ _ pos) = pos
statmentPos (IfElseSt _ _ _ pos) = pos
statmentPos (WhileSt _ _ pos) = pos
statmentPos (NextSt _ pos) = pos
statmentPos (BreakSt pos) = pos
statmentPos (FunSt _ pos) = pos
statmentPos (ReturnSt _ pos) = pos
statmentPos (WaitSt _ _ pos) = pos
                                            
funcName :: (Expression e) => Func e -> String
funcName BuildinFunc{biFuncName} = biFuncName
funcName UserFunc{uFuncName} = uFuncName

buildinFunc :: (Expression e) => Func e -> Bool                        
buildinFunc BuildinFunc{} = True
buildinFunc _ = False

funcRetType :: (Expression e) => Func e -> (EVariableType e)
funcRetType BuildinFunc{biFuncRetType} = biFuncRetType
funcRetType UserFunc{uFuncRetType} = uFuncRetType

funcArgsTypes BuildinFunc{biFuncArgs} = biFuncArgs
funcArgsTypes UserFunc{uFuncArgs} = map varType uFuncArgs

typeCanBe :: VarType -> VarType -> Bool
Int8Type `typeCanBe` Int16Type = True
Int8Type `typeCanBe` Int32Type = True
Int16Type `typeCanBe` Int32Type = True
typeCanBe a b = a == b

typeOfExpr :: SemExpr -> VarType
typeOfExpr (NumSemExpr n) | n<2^7 && n>(-2)^7 = Int8Type
                          | n<2^15 && n>(-2)^15 = Int16Type
                          | otherwise = Int32Type -- assume const size checked
typeOfExpr (BoolSemExpr _) = BoolType
typeOfExpr (TimeSemExpr _) = TimeType
typeOfExpr (StringSemExpr _) = StringType
typeOfExpr (VarSemExpr Var{varType}) = varType
typeOfExpr (FunSemExpr func _ ) = funcRetType func
typeOfExpr (SubSemExpr se) = typeOfExpr se
typeOfExpr (OpSemExpr _ _ _ opRetType) = opRetType
typeOfExpr VoidSemExpr = VoidType
typeOfExpr (ArrayAccessSemExpr expr _) = subType
    where (ArrayType subType _) = typeOfExpr expr
                
isArrayType :: VarType -> Bool
isArrayType (ArrayType _ _) = True
isArrayType _ = False
              
posChildOf :: (Expression e) => ProgramPos e -> ProgramPos e -> Bool
posChildOf _ InGlobal = True
posChildOf (InFunction f) (InFunction g) = f == g
posChildOf (InFunction _) _ = False
posChildOf _ (InFunction _) = False
posChildOf a b = case b of
                 InGlobal -> True
                 InThread btn -> case a of
                                  InThread atn -> atn == btn
                                  InState atn _ -> atn == btn
                                  InGlobal -> False
                                  InFunction _ -> False
                 InState _ _ -> a == b
                 InFunction _ -> False
                               
opTypes :: Map.Map SynOper [SemOper]
opTypes = Map.fromList [(PlusSynOp, [NumPlusSemOp, TimePlusSemOp])
                       ,(MinusSynOp, [NumMinusSemOp, TimeMinusSemOp])
                       ,(MulSynOp, [NumMulSemOp])
                       ,(DivSynOp, [NumDivSemOp])
                       ,(EqlSynOp, [NumEqlSemOp, TimeEqlSemOp, BoolEqlSemOp])
                       ,(NEqlSynOp, [NumNEqlSemOp, TimeNEqlSemOp, BoolNEqlSemOp])
                       ,(GrSynOp, [NumGrSemOp, TimeGrSemOp])
                       ,(LsSynOp, [NumLsSemOp, TimeLsSemOp])
                       ,(GrEqSynOp, [NumGrEqlSemOp, TimeGrEqlSemOp])
                       ,(LsEqSynOp, [NumLsEqlSemOp, TimeLsEqlSemOp])
                       ,(AndSynOp, [BoolAndSemOp])
                       ,(OrSynOp, [BoolOrSemOp])
                       ,(XorSynOp, [BoolXorSemOp])]


semOpTypes :: [(SemOper, (VarType, VarType, VarType))]
semOpTypes = concat [[(NumPlusSemOp, (numType, numType, numType))
                     ,(NumMinusSemOp, (numType, numType, numType))
                     ,(NumMulSemOp, (numType, numType, numType))
                     ,(NumDivSemOp, (numType, numType, numType))]
                     | numType <- [Int8Type, Int16Type, Int32Type]]
             ++ [(NumEqlSemOp, (Int32Type, Int32Type, BoolType)) -- Int32 - most general numeric type
                ,(NumLsSemOp, (Int32Type, Int32Type, BoolType))
                ,(NumGrSemOp, (Int32Type, Int32Type, BoolType))
                ,(NumLsEqlSemOp, (Int32Type, Int32Type, BoolType))
                ,(NumGrEqlSemOp, (Int32Type, Int32Type, BoolType))
                ,(NumNEqlSemOp, (Int32Type, Int32Type, BoolType))
                ,(BoolAndSemOp, (BoolType, BoolType, BoolType))
                ,(BoolOrSemOp, (BoolType, BoolType, BoolType))
                ,(BoolXorSemOp, (BoolType, BoolType, BoolType))
                ,(BoolEqlSemOp, (BoolType, BoolType, BoolType))
                ,(BoolNEqlSemOp, (BoolType, BoolType, BoolType))
                ,(TimePlusSemOp, (TimeType, TimeType, TimeType))
                ,(TimeMinusSemOp, (TimeType, TimeType, TimeType))
                ,(TimeEqlSemOp, (TimeType, TimeType, BoolType))
                ,(TimeLsSemOp, (TimeType, TimeType, BoolType))
                ,(TimeGrSemOp, (TimeType, TimeType, BoolType))
                ,(TimeLsEqlSemOp, (TimeType, TimeType, BoolType))
                ,(TimeGrEqlSemOp, (TimeType, TimeType, BoolType))
                ,(TimeNEqlSemOp, (TimeType, TimeType, TimeType))]

buildinFuncs :: [Func SemExpr]
buildinFuncs = [BuildinFunc{biFuncName="getPortByte"
                           ,biFuncTargetName="getPortByte"
                           ,biFuncArgs=[Int32Type]
                           ,biFuncRetType=Int8Type}
               ,BuildinFunc{biFuncName="setPortByte"
                           ,biFuncTargetName="setPortByte"
                           ,biFuncArgs=[Int32Type, Int8Type]
                           ,biFuncRetType=VoidType}
               ,BuildinFunc{biFuncName="getPortBit"
                           ,biFuncTargetName="getPortBit"
                           ,biFuncArgs=[Int32Type, Int32Type]
                           ,biFuncRetType=BoolType}
               ,BuildinFunc{biFuncName="setPortBit"
                           ,biFuncTargetName="setPortBit"
                           ,biFuncArgs=[Int32Type, Int32Type, BoolType]
                           ,biFuncRetType=VoidType}
               ,BuildinFunc{biFuncName="взятьПортБайт"
                           ,biFuncTargetName="getPortByte"
                           ,biFuncArgs=[Int32Type]
                           ,biFuncRetType=Int8Type}
               ,BuildinFunc{biFuncName="устПортБайт"
                           ,biFuncTargetName="setPortByte"
                           ,biFuncArgs=[Int32Type, Int8Type]
                           ,biFuncRetType=VoidType}
               ,BuildinFunc{biFuncName="взятьПортБит"
                           ,biFuncTargetName="getPortBit"
                           ,biFuncArgs=[Int32Type, Int32Type]
                           ,biFuncRetType=BoolType}
               ,BuildinFunc{biFuncName="устПортБит"
                           ,biFuncTargetName="setPortBit"
                           ,biFuncArgs=[Int32Type, Int32Type, BoolType]
                           ,biFuncRetType=VoidType}]