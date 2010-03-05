{-# LANGUAGE ExistentialQuantification,NamedFieldPuns,DeriveDataTypeable #-}
module Language.Rfx.Error where
import Control.Exception
import Language.Rfx.Structures
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Data.Typeable
import Data.List(nub)

data RfxException = forall e . Exception e => RfxException e
     deriving Typeable

instance Show RfxException where
    show (RfxException e) = show e

instance Exception RfxException

rfxExceptionToException :: Exception e => e -> SomeException
rfxExceptionToException = toException . RfxException

rfxExceptionFromException :: Exception e => SomeException -> Maybe e
rfxExceptionFromException x = do
     RfxException a <- fromException x
     cast a

data LexException = LexException ParseError
                    deriving (Typeable)
                             
instance Exception LexException where
     toException = rfxExceptionToException
     fromException = rfxExceptionFromException

parsecMessage (SysUnExpect s) = "Unexpected " ++ s
parsecMessage (UnExpect s) = "ue " ++ s
parsecMessage (Expect s) = "e " ++ s
parsecMessage (Message s) = "m " ++ s
                     
instance Show LexException where
    show (LexException pe) = "Lexer error at "
      ++ (show $ errorPos pe) ++ " :"
      ++ (parsecMessage $ head $ errorMessages pe)

data SynException = SynException ParseError
                  deriving (Typeable)

instance Exception SynException where
     toException = rfxExceptionToException
     fromException = rfxExceptionFromException

instance Show SynException where
    show (SynException pe) = "Syn error: " ++ (parsecMessage $ head $ errorMessages pe)

data SemException = VarInVarInitSemExc (Var SynExpr)
                  | VarInitWrongTypeSemExc (Var SynExpr)
                  | VarAlreadyExistsSemExc (Var SynExpr)
                  | IfNotBoolSemExc (Statment SynExpr)
                  | WhileNotBoolSemExc (Statment SynExpr)
                  | AssignWrongType (Statment SynExpr)
                  | OpSemExc SynExpr
                    deriving (Typeable, Show)

instance Exception SemException where
    toException = rfxExceptionToException
    fromException = rfxExceptionFromException
