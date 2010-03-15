{-# LANGUAGE ExistentialQuantification,NamedFieldPuns,DeriveDataTypeable #-}
module Language.Rfx.Error where
import Control.Exception
import Language.Rfx.Structures
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Data.Typeable

class Lined a where
    getErrorLine :: a -> Int
    
data RfxException = forall e . (Exception e, Lined e) => RfxException e
     deriving Typeable

instance Show RfxException where
    show (RfxException e) = show e

instance Exception RfxException

instance Lined RfxException where
    getErrorLine (RfxException e) = getErrorLine e
    
rfxExceptionToException :: (Exception e, Lined e) => e -> SomeException
rfxExceptionToException = toException . RfxException

rfxExceptionFromException :: (Exception e, Lined e) => SomeException -> Maybe e
rfxExceptionFromException x = do
     RfxException a <- fromException x
     cast a

data LexException = LexException ParseError
                    deriving (Typeable)
                             
instance Exception LexException where
     toException = rfxExceptionToException
     fromException = rfxExceptionFromException

instance Lined LexException where
    getErrorLine (LexException parseError) = sourceLine $ errorPos $ parseError                     

parsecMessage (SysUnExpect s) = "Unexpected " ++ s
parsecMessage (UnExpect s) = "ue " ++ s
parsecMessage (Expect s) = "e " ++ s
parsecMessage (Message s) = "m " ++ s
                     
instance Show LexException where
    show (LexException pe) = "Lexer error at "
      ++ (show $ errorPos pe) ++ " :"
      ++ (parsecMessage $ head $ errorMessages pe)

data SynException = SynException ParseError
                  | VarNameTooLongSynExc String SourcePos
                  deriving (Typeable)

instance Exception SynException where
     toException = rfxExceptionToException
     fromException = rfxExceptionFromException

instance Show SynException where
    show (SynException pe) = "Syn error: " ++ (parsecMessage $ head $ errorMessages pe)
    show (VarNameTooLongSynExc varName pos) = "Syn error: Var name has too many parts: "
                                          ++ varName ++ " at " ++ (show pos)

instance Lined SynException where
    getErrorLine (SynException parseError) = sourceLine $ errorPos $ parseError
    getErrorLine (VarNameTooLongSynExc _ pos) = sourceLine pos
                             
data SemException = VarInVarInitSemExc (Var SynExpr)
                  | VarInitWrongTypeSemExc (Var SynExpr)
                  | VarAlreadyExistsSemExc (Var SynExpr) SourcePos
                  | NoSuchTypeSemExc String SourcePos
                  | NoSuchThreadSemExc String SourcePos
                  | NoSuchStateSemExc String String SourcePos
                  | IfNotBoolSemExc (Statment SynExpr) SourcePos
                  | WhileNotBoolSemExc (Statment SynExpr) SourcePos
                  | AssignWrongTypeSemExc (Statment SynExpr)
                  | OpSemExc SynExpr
                  | NoSuchVarSemExc VarName SourcePos
                  | NoSuchFuncSemExc FuncName SourcePos
                  | FuncCallWrongTypeSemExc (Func SemExpr) SourcePos
                  | NextNotInStateSemExc SourcePos
                  | ReturnWrongTypeSemExc SourcePos
                  | ReturnPathsSemExc (Func SynExpr)
                  | ThreadAlreadyExistsSemExc String
                  | StateAlreadyExistsSemExc String String
                    deriving Typeable

instance Exception SemException where
    toException = rfxExceptionToException
    fromException = rfxExceptionFromException

instance Show SemException where
    show (NoSuchVarSemExc varName pos) = case varName of
                                       VarName name -> "No such variable " ++ name ++ " at " ++ (show pos)
                                       LongVarName thName varName -> "No such variable " ++
                                                                        thName ++ "." ++ varName ++ " at "
                                                                        ++ (show pos)
    show (OpSemExc (OpSynExpr op _ _ pos)) = "Incorrect usage of operator " ++ (show op) ++ " at " ++ (show pos)
    show (VarAlreadyExistsSemExc (Var{varSourcePos, varName}) otherPos) = "Variable " ++ varName ++ " at "
                                                                 ++ (show varSourcePos) ++ " already defined at " ++ (show otherPos)
    show (VarInitWrongTypeSemExc (Var{varSourcePos, varName})) = "Wrong type of init expression of variable "
                                                                 ++ varName ++ " at " ++ (show varSourcePos)
    show (VarInVarInitSemExc (Var{varSourcePos, varName})) = "Variable in init expression of variable " ++ varName
                                                             ++ " at " ++ (show varSourcePos)
    show (NoSuchTypeSemExc typeName pos) = "No such type " ++ typeName ++ " at " ++ (show pos)
    show (NoSuchThreadSemExc thName pos) = "No such thread " ++ thName ++ " at " ++ (show pos)
    show (NoSuchStateSemExc thName stName pos) = "No such state " ++ stName ++ " in thread " ++ thName
                                                 ++ " at " ++ (show pos)
    show (NoSuchFuncSemExc (FuncName fn) pos) = "No such function " ++ fn ++ " at " ++ (show pos)
    show (FuncCallWrongTypeSemExc func pos) = "Wrong types of arguments of function " ++ (funcName func) ++ " at " ++ (show pos)
    show (AssignWrongTypeSemExc (AssignSt varName _ pos)) = case varName of
                                                            VarName _ -> "Wrong type of rvalue at " ++ (show pos)
                                                            LongVarName _ _ -> "Wrong type of rvalue at " ++ (show pos)
    show (NextNotInStateSemExc pos) = "Next statment not inside state at " ++ (show pos)
    show (ReturnWrongTypeSemExc pos) = "Wrong type of argument of return statment at " ++ (show pos)
    show (ReturnPathsSemExc func) = "Not all paths in fuction " ++ (funcName func) ++ " return value"
    show (ThreadAlreadyExistsSemExc thName) = "Thread " ++ thName ++ " already exists"
    show (StateAlreadyExistsSemExc thName stName) = "State " ++ stName ++ " in thread " ++ thName ++ " already exists"
    show _ = "Lolwut?"
                    
instance Lined SemException where
    getErrorLine (VarInVarInitSemExc (Var{varSourcePos})) = sourceLine varSourcePos
    getErrorLine (VarInitWrongTypeSemExc (Var{varSourcePos})) = sourceLine varSourcePos
    getErrorLine (VarAlreadyExistsSemExc (Var{varSourcePos}) _) = sourceLine varSourcePos
    getErrorLine (NoSuchTypeSemExc _ pos) = sourceLine pos 
    getErrorLine (OpSemExc (OpSynExpr _ _ _ pos)) = sourceLine pos
    getErrorLine (NoSuchFuncSemExc _ pos) = sourceLine pos
    getErrorLine (FuncCallWrongTypeSemExc _ pos) = sourceLine pos
    getErrorLine (NoSuchVarSemExc varName pos) = case varName of
                                               VarName _ -> sourceLine pos
                                               LongVarName _ _ -> sourceLine pos
    getErrorLine (NoSuchThreadSemExc _ pos) = sourceLine pos
    getErrorLine (NoSuchStateSemExc _ _ pos) = sourceLine pos
    getErrorLine (AssignWrongTypeSemExc (AssignSt varName _ pos)) = case varName of
                                                            VarName _ -> sourceLine pos
                                                            LongVarName _ _ -> sourceLine pos
    getErrorLine (NextNotInStateSemExc pos) = sourceLine pos
    getErrorLine (ReturnWrongTypeSemExc pos) = sourceLine pos
    getErrorLine (ReturnPathsSemExc UserFunc{uFuncPos}) = sourceLine uFuncPos 
    getErrorLine _ = 0