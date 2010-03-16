{-# LANGUAGE ExistentialQuantification,NamedFieldPuns,DeriveDataTypeable #-}
module Language.Rfx.Error where
import Control.Exception
import Language.Rfx.Structures
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Data.Typeable
import Text.Printf
import System.IO.Unsafe

__ :: String -> String    
__ = id
    
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

parsecMessage :: Message -> String
parsecMessage (SysUnExpect s) = printf (__ "unexpected %s") s
parsecMessage (UnExpect s) = printf (__ "unexpected %s") s
parsecMessage (Expect s) = printf (__ "expected %s") s
parsecMessage (Message s) = printf (__ "message %s") s
                     
instance Show LexException where
    show (LexException pe) =
        printf (__ "Lexer error: %s at %s.")
                   (parsecMessage $ head $ errorMessages pe)
                   (show $ errorPos pe)

data SynException = SynException ParseError
                  | VarNameTooLongSynExc String SourcePos
                  deriving (Typeable)

instance Exception SynException where
     toException = rfxExceptionToException
     fromException = rfxExceptionFromException

instance Show SynException where
    show (SynException pe) = printf (__ "Syntax error: %s.")
                             (parsecMessage $ head $ errorMessages pe)

    show (VarNameTooLongSynExc varName pos) = printf (__ "Syntax error: variable %s has too many parts at %s.")
                                              varName (show pos)

instance Lined SynException where
    getErrorLine (SynException parseError) = sourceLine $ errorPos $ parseError
    getErrorLine (VarNameTooLongSynExc _ pos) = sourceLine pos
                             
data SemException = VarInVarInitSemExc (Var SynExpr)
                  | VarInitWrongTypeSemExc (Var SynExpr)
                  | VarAlreadyExistsSemExc (Var SynExpr) SourcePos
                  | NoSuchTypeSemExc String SourcePos
                  | NoSuchThreadSemExc String SourcePos
                  | NoSuchStateSemExc String String SourcePos
                  | NeedBoolSemExc (Statment SynExpr)
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

fullVarName (VarName name) = name
fullVarName (LongVarName thName varName) = thName ++ (__ ".") ++ varName

instance Show SemException where
    show (NoSuchVarSemExc varName pos) =
        printf (__ "Error: no such variable %s at %s.")
                   (fullVarName varName) (show pos)
    show (OpSemExc (OpSynExpr op _ _ pos)) =
        printf (__ "Error: incorrect usage of operator %s at %s.")
                   (show op) (show pos)
    show (VarAlreadyExistsSemExc (Var{varSourcePos, varName}) otherPos) =
        printf (__ "Error: variable %s at position %s already defined at %s.")
               varName (show varSourcePos) (show otherPos)
    show (VarInitWrongTypeSemExc (Var{varSourcePos, varName})) =
        printf (__ "Error: wrong type of initialization expression of variable %s at %s")
               varName (show varSourcePos)
    show (VarInVarInitSemExc (Var{varSourcePos, varName})) =
        printf (__ "Error: usage variables in initialization exression prohibited; variable %s at %s.")
               varName (show varSourcePos)
    show (NoSuchTypeSemExc typeName pos) =
        printf (__ "Error: no such type %s at %s.")
               typeName (show pos)
    show (NoSuchThreadSemExc thName pos) =
        printf (__ "Error: no such thread %s at %s.")
               thName (show pos)
    show (NoSuchStateSemExc thName stName pos) =
        printf (__ "Error: no such state %s in thread %s at %s.")
               stName thName (show pos)
    show (NoSuchFuncSemExc (FuncName fn) pos) =
        printf (__ "Error: no such function %s at %s.")
               fn (show pos)
    show (FuncCallWrongTypeSemExc func pos) =
        printf (__ "Error: wrong types of arguments of function %s at %s.")
               (funcName func) (show pos)
    show (AssignWrongTypeSemExc (AssignSt varName _ pos)) =
        printf (__ "Error: wrong type of expression in assigment to %s at %s.")
               (fullVarName varName) (show pos)
    show (NextNotInStateSemExc pos) =
        printf (__ "Error: Next statment outside of state clause at %s.")
               (show pos)
    show (ReturnWrongTypeSemExc pos) =
        printf (__ "Error: wrong type of argument of return statment at %s.")
               (show pos)
    show (ReturnPathsSemExc func) =
        printf (__ "Error: not all executoin paths in function %s return value.")
               (funcName func)
    show (ThreadAlreadyExistsSemExc thName) =
        printf (__ "Error: multiple definition of thread %s.")
               thName
    show (StateAlreadyExistsSemExc thName stName) =
        printf (__ "Error: multiple definition of state %s in thread %s.")
               stName thName
    show (NeedBoolSemExc st) =
        printf (__ "Error: statment argument type must be boolean at %s.")
               (show $ statmentPos st)
    show _ = (__ "Lolwut?")
                    
instance Lined SemException where
    getErrorLine (VarInVarInitSemExc (Var{varSourcePos}))       = sourceLine varSourcePos
    getErrorLine (VarInitWrongTypeSemExc (Var{varSourcePos}))   = sourceLine varSourcePos
    getErrorLine (VarAlreadyExistsSemExc (Var{varSourcePos}) _) = sourceLine varSourcePos
    getErrorLine (NoSuchTypeSemExc _ pos)                       = sourceLine pos 
    getErrorLine (OpSemExc (OpSynExpr _ _ _ pos))               = sourceLine pos
    getErrorLine (NoSuchFuncSemExc _ pos)                       = sourceLine pos
    getErrorLine (FuncCallWrongTypeSemExc _ pos)                = sourceLine pos
    getErrorLine (NoSuchVarSemExc _ pos)                        = sourceLine pos
    getErrorLine (NoSuchThreadSemExc _ pos)                     = sourceLine pos
    getErrorLine (NoSuchStateSemExc _ _ pos)                    = sourceLine pos
    getErrorLine (AssignWrongTypeSemExc (AssignSt _ _ pos))     = sourceLine pos
    getErrorLine (NextNotInStateSemExc pos)                     = sourceLine pos
    getErrorLine (ReturnWrongTypeSemExc pos)                    = sourceLine pos
    getErrorLine (ReturnPathsSemExc UserFunc{uFuncPos})         = sourceLine uFuncPos
    getErrorLine (NeedBoolSemExc st)                            = sourceLine $ statmentPos st
    getErrorLine _                                              = 0