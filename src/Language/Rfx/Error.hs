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
                  | VarAlreadyExistsSemExc (Var SynExpr)
                  | NoSuchTypeSemExc (Var SynExpr) String
                  | IfNotBoolSemExc (Statment SynExpr)
                  | WhileNotBoolSemExc (Statment SynExpr)
                  | AssignWrongType (Statment SynExpr)
                  | OpSemExc SynExpr
                  | NoSuchVarSemExc VarName
                    deriving (Typeable, Show)

instance Exception SemException where
    toException = rfxExceptionToException
    fromException = rfxExceptionFromException

instance Lined SemException where
    getErrorLine (VarInVarInitSemExc (Var{varSourcePos})) = sourceLine varSourcePos + 1
    getErrorLine (VarInitWrongTypeSemExc (Var{varSourcePos})) = sourceLine varSourcePos + 1
    getErrorLine (VarAlreadyExistsSemExc (Var{varSourcePos})) = sourceLine varSourcePos + 1
    getErrorLine (NoSuchTypeSemExc (Var{varSourcePos}) _) = sourceLine varSourcePos + 1
    getErrorLine (OpSemExc (OpSynExpr _ _ _ pos)) = sourceLine pos + 1
    getErrorLine (NoSuchVarSemExc varName) = case varName of
                                               VarName _ pos -> sourceLine pos
                                               LongVarName _ _ pos -> sourceLine pos
    getErrorLine _ = 0