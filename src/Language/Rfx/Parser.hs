module Language.Rfx.Parser(parseProgram)
where
import Language.Rfx.Tokens
import Language.Rfx.Structures
import Language.Rfx.Util
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import qualified Data.Map as Map

data ParserState = ParserState
    {
      parserVars :: Map.Map String Var
    , parserPos :: ProgramPos
    }

defaultParserState :: ParserState                 
defaultParserState = ParserState Map.empty InGlobal                   
                   
type TokenParser = GenParser Token ParserState

parseProgram :: [Token] -> Program
parseProgram ts = case runParser programParser defaultParserState "" ts of
                        Left err -> error $ "Lexer error\n" ++ show err
                        Right t -> Program t

tokenTestParser :: (Token -> Bool) -> TokenParser Token
tokenTestParser test = token showTok posFromTok testTok
    where
      showTok t = show t
      posFromTok _ = newPos "" 0 0
      testTok t = if test t then Just t else Nothing

tokenParser :: Token -> TokenParser Token
tokenParser t = tokenTestParser (==t)

identifierParser :: TokenParser Token
identifierParser = tokenTestParser (\x -> case x of
                                           (IdentifierToken _) -> True
                                           _ -> False)

numberParser :: TokenParser Token
numberParser = tokenTestParser(\x -> case x of
                                           (NumberToken _) -> True
                                           _ -> False)

programParser :: TokenParser [Thread]
programParser =do
  many $ try varDefParser
  many threadParser

varDefParser :: TokenParser ()
varDefParser = do
  (IdentifierToken varTypeName) <- identifierParser
  (IdentifierToken vn) <- identifierParser
  tokenParser AssignToken
  expr <- exprParser
  tokenParser SemicolonToken
  case getVarType varTypeName of
    Nothing -> return $ error $ "No such type " ++ varTypeName ++ "\n"
    Just tp -> do
      addVar vn expr tp

threadParser :: TokenParser Thread
threadParser = do
  tokenParser ThreadToken
  (IdentifierToken thName) <- identifierParser
  enterThread thName
  tokenParser WhereToken
  many $ try varDefParser
  sts <- manyTill stateParser $ do
                 tokenParser EndToken
                 tokenParser SemicolonToken
  leave
  return $ Thread thName sts
         
stateParser :: TokenParser ThreadState
stateParser = do
  tokenParser StateToken
  (IdentifierToken stName) <- identifierParser
  enterState stName
  tokenParser WhereToken
  many $ try varDefParser
  sts <- manyTill statmentParser $ do
                tokenParser EndToken
                tokenParser SemicolonToken
  leave
  return $ ThreadState stName sts

statmentParser :: TokenParser Statment
statmentParser = do
  statment <- choice [try assignParser]
  tokenParser SemicolonToken
  return statment

assignParser :: TokenParser Statment
assignParser = do
  (IdentifierToken vn) <- identifierParser
  var <- getVar vn
  tokenParser AssignToken
  expr <- exprParser
  return $ AssignSt var expr

exprParser :: TokenParser Expr
exprParser = do
  (NumberToken n) <- numberParser
  return $ NumExpr n

-- State funs
addVar :: String -> Expr -> VarType -> TokenParser ()
addVar vn iv vt = do
  (ParserState vars pos) <- getState
  state <- getState
  if Map.member vn vars
    then return $ error $ "Variable " ++ vn ++ " already defined\n"
    else do
      setState $ state{parserVars = Map.insert vn (Var vn iv pos vt) vars}
              
getVar :: String -> TokenParser Var
getVar vn = do
  (ParserState vars pos) <- getState
  let maybeVar = Map.lookup vn vars
  case maybeVar of
    Nothing -> error $ "Variable " ++ vn ++ " not defined yet\n"
    Just var@(Var _ _ scope _) -> return $ if pos `posChild` scope
                                      then var
                                      else error $ "Variable " ++ vn ++ " not in this scope\n"

setParserPos :: ProgramPos -> TokenParser ()
setParserPos pos = do
  state <- getState
  setState $ state{parserPos = pos}
         
enterThread :: String -> TokenParser ()
enterThread tn = setParserPos $ InThread tn

enterState :: String -> TokenParser ()
enterState stName = do
  (ParserState _ (InThread thName)) <- getState
  setParserPos $ InState thName stName

leave :: TokenParser ()
leave = do
  (ParserState _ comPos) <- getState
  state <- getState
  case comPos of
    InGlobal -> setParserPos InGlobal
    (InThread _) -> setParserPos InGlobal
    (InState thName _) -> setParserPos $ InThread thName

