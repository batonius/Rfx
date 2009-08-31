module Language.Rfx.Parser(parseProgram)
where
import Language.Rfx.Tokens
import Language.Rfx.Structures
import Language.Rfx.Util
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import qualified Data.Set as Set

data ParserState = ParserState
    {
      parserVars :: Set.Set Var
    , parserPos :: ProgramPos
    }

defaultParserState :: ParserState                 
defaultParserState = ParserState Set.empty InGlobal                   
                   
type TokenParser = GenParser Token ParserState

parseProgram :: [Token] -> Program
parseProgram ts = case runParser programParser defaultParserState "" ts of
                        Left err -> error $ "Lexer error\n" ++ show err
                        Right p ->  p

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

programParser :: TokenParser Program
programParser =do
  many $ try varDefParser
  ths <- many threadParser
  state <- getState
  return $ Program ths $ parserVars state

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
  statment <- choice [ try assignParser
                     , try breakParser
                     , try whileParser
                     , try ifParser]
  tokenParser SemicolonToken
  return statment

breakParser :: TokenParser Statment
breakParser = do
  tokenParser BreakToken
--  tokenParser SemicolonToken
  return $ BreakSt

whileParser :: TokenParser Statment
whileParser = do
  tokenParser WhileToken
  expr <- exprParser
  tokenParser DoToken
  sts <- manyTill statmentParser $ do
                tokenParser EndToken
--                tokenParser SemicolonToken
  return $ WhileSt expr sts

ifParser :: TokenParser Statment
ifParser = do
  tokenParser IfToken
  expr <- exprParser
  tokenParser ThenToken
  sts <- manyTill statmentParser $ choice [ lookAhead $ do
                                              tokenParser EndToken 
                                       --       tokenParser SemicolonToken
                                          , lookAhead $ do
                                              tokenParser ElseToken]
  next <- choice [ do
                   tokenParser EndToken
          --         tokenParser SemicolonToken
                 , tokenParser ElseToken]
  if next /= ElseToken
      then return $ IfSt expr sts
      else do
        sts2 <- manyTill statmentParser $ do
                  tokenParser EndToken
--                  tokenParser SemicolonToken
        return $ IfElseSt expr sts sts2
         
assignParser :: TokenParser Statment
assignParser = do
  (IdentifierToken vn) <- identifierParser
  var <- getVar vn
  tokenParser AssignToken
  expr <- exprParser
  return $ AssignSt var expr

exprParser :: TokenParser Expr
exprParser = do
  expr <- choice [ try opExprParser
                 , try numExprParser
                 , try varExprParser
                 , try subExprParser]
  return expr

numExprParser :: TokenParser Expr
numExprParser = do
  (NumberToken n) <- numberParser
  return $ NumExpr n

subExprParser :: TokenParser Expr
subExprParser = do
  tokenParser LParToken
  expr <- exprParser
  tokenParser RParToken
  return $ SubExpr expr 

varExprParser :: TokenParser Expr
varExprParser = do
  (IdentifierToken vn) <- identifierParser
  var <- getVar vn
  return $ VarExpr var

tokenOps :: [(Token, Oper)]
tokenOps = [ (PlusToken, PlusOp)
           , (MinusToken, MinusOp)
           , (AsteriskToken, MulOp)
           , (SlashToken, DivOp)
           , (EqualToken, EqualityOp)
           , (GrToken, GrOp)
           , (LsToken, LsOp)
           , (GrEqToken, GrEqOp)
           , (LsEqToken, LsEqOp)]
         
opExprParser :: TokenParser Expr
opExprParser = do
  lexpr <- choice [try numExprParser
                 , try varExprParser
                 , try subExprParser]
  op <- choice [try $ tokenParser tok
                   | (tok, _) <- tokenOps]
  rexpr <- exprParser
  case lookup op tokenOps of
    Nothing -> return $ error"Operator unknown\n"
    Just o -> return $ OpExpr o lexpr rexpr
         
-- State funs
addVar :: String -> Expr -> VarType -> TokenParser ()
addVar vn iv vt = do
  state <- getState
  let vars = parserVars state
  let pos = parserPos state
  let newVar = (Var vn iv pos vt)
  if Set.member newVar vars
    then return $ error $ "Variable " ++ vn ++ " already defined in this scope\n"
    else do
      setState $ state{parserVars = Set.insert newVar vars}
              
getVar :: String -> TokenParser Var
getVar vn = do
  state <- getState
  let vars = parserVars state
  let pos = parserPos state
  let filteredVars = Set.toList $ Set.filter (\(Var name _ vs _) ->
                                                  (name==vn) && (pos `posChildOf` vs)) vars
  case length filteredVars of
    0 -> return $ error $ "Variable " ++ vn ++ " not defined in this context\n"
    1 -> return $ head filteredVars
    _ -> return $ error $ "You shouln't see this error message\n"

setParserPos :: ProgramPos -> TokenParser ()
setParserPos pos = do
  state <- getState
  setState $ state{parserPos = pos}
         
enterThread :: String -> TokenParser ()
enterThread tn = setParserPos $ InThread tn

enterState :: String -> TokenParser ()
enterState stName = do
  state <- getState
  let (InThread thName) = (parserPos state)
  setParserPos $ InState thName stName

leave :: TokenParser ()
leave = do
  state <- getState
  let comPos = parserPos state
  case comPos of
    InGlobal -> setParserPos InGlobal
    (InThread _) -> setParserPos InGlobal
    (InState thName _) -> setParserPos $ InThread thName

