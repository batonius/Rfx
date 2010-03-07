module Language.Rfx.Parser(parseProgram)
where
import Language.Rfx.Tokens
import Language.Rfx.Structures
import Language.Rfx.Util
import Language.Rfx.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Control.Exception hiding (try)

type TokenParser = GenParser (Tagged Token) [Var SynExpr] -- ParserState

parseProgram :: [Tagged Token] -> Program SynExpr
parseProgram ts = case runParser programParser [] "" ts of
                        Left err -> throw $ SynException err
                        Right p ->  p
                                   
programParser :: TokenParser (Program SynExpr)
programParser = do
  vars <- many $ try $ varDefParser
  ths <- many1 threadParser
  thVars <- getState
  return $ Program{programThreads=ths, programVars=(setVarsScope InGlobal vars)++thVars}

addVars :: [Var SynExpr] -> TokenParser ()
addVars vars = do
  oldVars <- getState
  setState (oldVars ++ vars)
                        
varDefParser :: TokenParser (Var SynExpr)
varDefParser = do
  (Tagged pos (IdentifierToken varTypeName)) <- taggedIdentifierParser
  setPosition pos
  (IdentifierToken vn) <- identifierParser
  tokenParser AssignToken
  initExpr <- exprParser
  tokenParser SemicolonToken
  case getVarType varTypeName of
    Nothing -> return $ error $ "No such type " ++ varTypeName ++ "\n"
    Just tp -> do
      return Var{varName=vn
                ,varType=tp
                ,varInitValue=initExpr
                ,varScope=InGlobal
                ,varSourcePos=pos}

threadParser :: TokenParser (Thread SynExpr)
threadParser = do
  tokenParser ThreadToken
  (IdentifierToken thName) <- identifierParser
  tokenParser WhereToken
  vars <- many $ try varDefParser 
  sts <- manyTill stateParser $ do
                 tokenParser EndToken
                 tokenParser SemicolonToken
  let thread = Thread{threadName=thName,threadStates=map fst sts}
  sequence_ [addVars $ setVarsScope (InState thread state) stVars              
    | (state, stVars) <- sts]
  addVars $ setVarsScope (InThread thread) vars
  return thread

stateParser :: TokenParser (ThreadState SynExpr, [Var SynExpr])
stateParser = do
  tokenParser StateToken
  (IdentifierToken stName) <- identifierParser
  tokenParser WhereToken
  vars <- many $ try varDefParser
  sts <- manyTill statmentParser $ do
                tokenParser EndToken
                tokenParser SemicolonToken
  let state = ThreadState{stateName=stName
                         ,stateStatments=sts}
  return $ (state, vars)

statmentParser :: TokenParser (Statment SynExpr)
statmentParser = do
  statment <- choice [ try assignParser
                    , try breakParser
                    , try whileParser
                    , try ifParser
                    , try nextParser
                    , try funParser]
  tokenParser SemicolonToken
  return statment

breakParser :: TokenParser (Statment SynExpr)
breakParser = do
  tokenParser BreakToken
  return $ BreakSt

whileParser :: TokenParser (Statment SynExpr)
whileParser = do
  tokenParser WhileToken
  expr <- exprParser
  tokenParser DoToken
  sts <- manyTill statmentParser $ tokenParser EndToken
  return $ WhileSt expr sts

ifParser :: TokenParser (Statment SynExpr)
ifParser = do
  tokenParser IfToken
  expr <- exprParser
  tokenParser ThenToken
  sts <- manyTill statmentParser $ choice [ lookAhead $ tokenParser EndToken
                                          , lookAhead $ tokenParser ElseToken]
  next <- choice [ tokenParser EndToken
                 , tokenParser ElseToken]
  if next /= ElseToken
    then return $ IfSt expr sts
    else do
      sts2 <- manyTill statmentParser $ tokenParser EndToken
      return $ IfElseSt expr sts sts2

nextParser :: TokenParser (Statment SynExpr)
nextParser = do
  tokenParser NextToken
  (IdentifierToken stName) <- identifierParser
  return $ NextSt $ ThreadState stName []

assignParser :: TokenParser (Statment SynExpr)
assignParser = do
  varName <- varNameParser
  tokenParser AssignToken
  expr <- exprParser
  return $ AssignSt varName expr

funParser :: TokenParser (Statment SynExpr)
funParser = do
  fun <- funExprParser
  return $ FunSt fun

exprParser :: TokenParser SynExpr
exprParser = do
  expr <- choice [ try opExprParser
                , try numExprParser
                , try funExprParser
                , try varExprParser
                , try subExprParser
                , try stringExprParser]
  return expr

funExprParser :: TokenParser SynExpr
funExprParser = do
  (IdentifierToken funName) <- identifierParser
  tokenParser LParToken
  args <- sepBy exprParser (tokenParser CommaToken)
  tokenParser RParToken
  return $ FunSynExpr funName args

numExprParser :: TokenParser SynExpr
numExprParser = do
  (NumberToken n) <- numberParser
  return $ NumSynExpr n

stringExprParser :: TokenParser SynExpr
stringExprParser = do
  (StringToken s) <- stringParser
  return $ StringSynExpr s
         
subExprParser :: TokenParser SynExpr
subExprParser = do
  tokenParser LParToken
  expr <- exprParser
  tokenParser RParToken
  return $ SubSynExpr expr

varExprParser :: TokenParser SynExpr
varExprParser = do
  varName <- varNameParser
  return $ VarSynExpr varName

tokenOps :: [(Token, SynOper)]
tokenOps = [ (PlusToken, PlusSynOp)
           , (MinusToken, MinusSynOp)
           , (AsteriskToken, MulSynOp)
           , (SlashToken, DivSynOp)
           , (EqualToken, EqlSynOp)
           , (GrToken, GrSynOp)
           , (LsToken, LsSynOp)
           , (GrEqToken, GrEqSynOp)
           , (LsEqToken, LsEqSynOp)]

opExprParser :: TokenParser SynExpr
opExprParser = do
  lexpr <- choice [try numExprParser
                 , try varExprParser
                 , try subExprParser]
  op <- choice [try $ tokenParser tok
                   | (tok, _) <- tokenOps]
  rexpr <- exprParser
  case lookup op tokenOps of
    Nothing -> return $ error"Operator unknown\n"
    Just o -> return $ OpSynExpr o lexpr rexpr

varNameParser :: TokenParser VarName
varNameParser = do
  varNameParts <- sepBy1 identifierParser (tokenParser DotToken)
  case length varNameParts of
    1 -> do
      let (IdentifierToken varId) = head varNameParts
      return $ VarName varId
    2 -> do
      let (IdentifierToken thId) = head varNameParts
      let (IdentifierToken varId) = varNameParts !! 1
      return $ LongVarName thId varId
    _ -> return $ error "Too long variable name"

-- -- Helper funs
tokenTestParser :: (Token -> Bool) -> TokenParser Token
tokenTestParser test = token showTok posFromTok testTok
    where
      showTok t = show t
      posFromTok _ = newPos "" 0 0
      testTok :: (Tagged Token) -> Maybe Token
      testTok t = if (test.value) t then Just (value t) else Nothing

taggedTokenTestParser :: (Token -> Bool) -> TokenParser (Tagged Token)
taggedTokenTestParser test = token showTok posFromTok testTok
    where
      showTok t = show t
      posFromTok _ = newPos "" 0 0
      testTok :: (Tagged Token) -> Maybe (Tagged Token)
      testTok t = if (test.value) t then Just t else Nothing

tokenParser :: Token -> TokenParser Token
tokenParser t = tokenTestParser (==t)

identifierParser :: TokenParser Token
identifierParser = tokenTestParser (\x -> case x of
                                           (IdentifierToken _) -> True
                                           _ -> False)

taggedIdentifierParser :: TokenParser (Tagged Token)
taggedIdentifierParser = taggedTokenTestParser (\x -> case x of
                                                     (IdentifierToken _) -> True
                                                     _ -> False)
                                                
numberParser :: TokenParser Token
numberParser = tokenTestParser(\x -> case x of
                                           (NumberToken _) -> True
                                           _ -> False)

stringParser :: TokenParser Token
stringParser = tokenTestParser(\x -> case x of
                                      (StringToken _) -> True
                                      _ -> False)

setVarsScope :: ProgramPos SynExpr -> [Var SynExpr] -> [Var SynExpr]
setVarsScope newScope = map (\var -> var{varScope=newScope})
