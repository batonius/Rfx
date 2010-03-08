{-# LANGUAGE ExistentialQuantification,NamedFieldPuns,DeriveDataTypeable #-}
module Language.Rfx.Error where
import Control.Exception
import Language.Rfx.Structures
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Data.Typeable
import Data.List(nub)

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
                  | NoSuchTypeSemExc (Var SynExpr) String
                  | NoSuchThreadSemExc String SourcePos
                  | NoSuchStateSemExc String String SourcePos
                  | IfNotBoolSemExc (Statment SynExpr)
                  | WhileNotBoolSemExc (Statment SynExpr)
                  | AssignWrongType (Statment SynExpr)
                  | OpSemExc SynExpr
                  | NoSuchVarSemExc VarName
                    deriving Typeable

instance Exception SemException where
    toException = rfxExceptionToException
    fromException = rfxExceptionFromException

instance Show SemException where
    show (NoSuchVarSemExc varName) = case varName of
                                       VarName name pos -> "No such variable " ++ name ++ " at " ++ (show pos)
                                       LongVarName thName varName pos -> "No such variable" ++
                                                                        thName ++ "." ++ varName ++ " at "
                                                                        ++ (show pos)
    show (OpSemExc (OpSynExpr op _ _ pos)) = "Incorrect usage of operator " ++ (show op) ++ " at " ++ (show pos)
    show (VarAlreadyExistsSemExc (Var{varSourcePos, varName}) otherPos) = "Variable " ++ varName ++ " at "
                                                                 ++ (show varSourcePos) ++ " already defined at " ++ (show otherPos)
    show (VarInitWrongTypeSemExc (Var{varSourcePos, varName})) = "Wrong type of init expression of variable "
                                                                 ++ varName ++ " at " ++ (show varSourcePos)
    show (VarInVarInitSemExc (Var{varSourcePos, varName})) = "Variable in init expression of variable " ++ varName
                                                             ++ " at " ++ (show varSourcePos)
    show (NoSuchTypeSemExc (Var{varName, varSourcePos}) varTypeName) = "No such type " ++ (varTypeName)
                           ++ ", variable " ++ varName ++ " at " ++ (show varSourcePos)
    show (NoSuchThreadSemExc thName pos) = "No such thread " ++ thName ++ " at " ++ (show pos)
    show (NoSuchStateSemExc thName stName pos) = "No such state " ++ stName ++ " in thread " ++ thName
                                                 ++ " at " ++ (show pos)
    show (AssignWrongType (AssignSt varName _)) = case varName of
                                                            VarName _ pos -> "Wrong type of rvalue at " ++ (show pos)
                                                            LongVarName _ _ pos -> "Wrong type of rvalue at " ++ (show pos)
    show _ = "Lolwut?"
                    
instance Lined SemException where
    getErrorLine (VarInVarInitSemExc (Var{varSourcePos})) = sourceLine varSourcePos + 1
    getErrorLine (VarInitWrongTypeSemExc (Var{varSourcePos})) = sourceLine varSourcePos + 1
    getErrorLine (VarAlreadyExistsSemExc (Var{varSourcePos}) _) = sourceLine varSourcePos + 1
    getErrorLine (NoSuchTypeSemExc (Var{varSourcePos}) _) = sourceLine varSourcePos + 1
    getErrorLine (OpSemExc (OpSynExpr _ _ _ pos)) = sourceLine pos + 1
    getErrorLine (NoSuchVarSemExc varName) = case varName of
                                               VarName _ pos -> sourceLine pos
                                               LongVarName _ _ pos -> sourceLine pos
    getErrorLine (NoSuchThreadSemExc _ pos) = sourceLine pos
    getErrorLine (NoSuchStateSemExc _ _ pos) = sourceLine pos
    getErrorLine (AssignWrongType (AssignSt varName _)) = case varName of
                                                            VarName _ pos -> sourceLine pos
                                                            LongVarName _ _ pos -> sourceLine pos
    getErrorLine _ = 0