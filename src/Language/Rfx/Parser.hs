{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Parser(parseProgram)
where
import Language.Rfx.Tokens
import Language.Rfx.Structures
import Language.Rfx.Error
import Text.ParserCombinators.Parsec
import Control.Exception hiding (try)

type TokenParser = GenParser (Tagged Token) ParserState

data ParserState = ParserState
                 {
                    parserVars :: [Var SynExpr]
                   ,parserLastWait :: Int
                 }

parserState = ParserState [] 1
    
parseProgram :: [Tagged Token] -> Program SynExpr
parseProgram ts = case runParser programParser parserState "" ts of
                        Left err -> throw $ SynException err
                        Right p ->  p

programParser :: TokenParser (Program SynExpr)
programParser = do
  vars <- many $ try $ (varDefParser InGlobal False)
  funcs <- many $ try $ funcDefParser
  programThreads <- many1 threadParser
  ParserState{parserVars} <- getState
  return $ Program{programThreads
                  ,programVars=vars++parserVars
                  ,programFuncs=funcs}

addVars :: [Var SynExpr] -> TokenParser ()
addVars vars = do
  state@ParserState{parserVars} <- getState
  setState state{parserVars=parserVars ++ vars}

varDefParser :: ProgramPos SynExpr -> Bool -> TokenParser (Var SynExpr)
varDefParser scope arg = do
  typeName <- typeNameParser
  varSourcePos <- getPosition
  (IdentifierToken varName) <- identifierParser
  next <- choice [tokenParser AssignToken, tokenParser SemicolonToken]
  varInitValue <- if next == SemicolonToken then return VoidSynExpr
                 else do
                   viv <- exprParser
                   tokenParser SemicolonToken
                   return viv
  return Var{varName
            ,varType=typeName
            ,varInitValue
            ,varScope=scope
            ,varSourcePos
            ,varArg=arg}

funcDefParser :: TokenParser (Func SynExpr)
funcDefParser = do
    (IdentifierToken funcRetType) <- identifierParser
    funcSourcePos <- getPosition
    (IdentifierToken funcName) <- identifierParser
    let funcScope = InFunction $ FuncName funcName
    tokenParser LParToken
    args <- (flip sepBy) (tokenParser CommaToken) $ do
      (IdentifierToken varTypeName) <- identifierParser
      (IdentifierToken varName) <- identifierParser
      return $ Var{varName
                  ,varType=VarTypeName varTypeName
                  ,varScope=funcScope
                  ,varSourcePos=funcSourcePos
                  ,varInitValue=VoidSynExpr
                  ,varArg=True}
    tokenParser RParToken
    localVars <- many $ try (varDefParser funcScope False)
    statments <- manyTill statmentParser $ do
      tokenParser EndToken
      tokenParser SemicolonToken
    addVars args
    addVars localVars
    return $ UserFunc{uFuncName = funcName
                     ,uFuncRetType = VarTypeName funcRetType
                     ,uFuncArgs = args
                     ,uFuncStatments = statments
                     ,uFuncPos = funcSourcePos}

threadParser :: TokenParser (Thread SynExpr)
threadParser = do
  tokenParser ThreadToken
  (IdentifierToken threadName) <- identifierParser
  let thName = ThreadName threadName
  let threadScope = InThread thName
  tokenParser WhereToken
  vars <- many $ try (varDefParser threadScope False)
  addVars vars
  threadStates <- manyTill (stateParser thName) $ do
    tokenParser EndToken
    tokenParser SemicolonToken
  return Thread{threadName,threadStates}

stateParser :: ThreadName -> TokenParser (ThreadState SynExpr)
stateParser threadName = do
  tokenParser StateToken
  (IdentifierToken stateName) <- identifierParser
  let stateScope = InState threadName $ StateName stateName
  tokenParser WhereToken
  vars <- many $ try (varDefParser stateScope False)
  addVars vars
  zeroLastWait
  stateStatments <- manyTill statmentParser $ do
    tokenParser EndToken
    tokenParser SemicolonToken
  return $ ThreadState{stateName,stateStatments}

statmentParser :: TokenParser (Statment SynExpr)
statmentParser = do
  statment <- choice [try assignParser
                    ,try breakParser
                    ,try whileParser
                    ,try ifParser
                    ,try nextParser
                    ,try funParser
                    ,try returnParser
                    ,try waitParser]
  tokenParser SemicolonToken
  return statment

breakParser :: TokenParser (Statment SynExpr)
breakParser = do
  tokenParser BreakToken
  pos <- getPosition
  return $ BreakSt pos

whileParser :: TokenParser (Statment SynExpr)
whileParser = do
  tokenParser WhileToken
  pos <- getPosition
  expr <- exprParser
  tokenParser DoToken
  sts <- manyTill statmentParser $ tokenParser EndToken
  return $ WhileSt expr sts pos

ifParser :: TokenParser (Statment SynExpr)
ifParser = do
  tokenParser IfToken
  pos <- getPosition
  expr <- exprParser
  tokenParser ThenToken
  sts <- manyTill statmentParser $ choice [lookAhead $ tokenParser EndToken
                                          ,lookAhead $ tokenParser ElseToken]
  next <- choice [tokenParser EndToken
                 ,tokenParser ElseToken]
  if next /= ElseToken
    then return $ IfSt expr sts pos
    else do
      sts2 <- manyTill statmentParser $ tokenParser EndToken
      return $ IfElseSt expr sts sts2 pos

nextParser :: TokenParser (Statment SynExpr)
nextParser = do
  tokenParser NextToken
  pos <- getPosition
  (IdentifierToken stName) <- identifierParser
  return $ NextSt (StateName stName) pos

waitParser :: TokenParser (Statment SynExpr)
waitParser = do
  tokenParser WaitToken
  pos <- getPosition
  expr <- exprParser
  lastWait <- incLastWait
  return $ WaitSt expr lastWait pos
         
assignParser :: TokenParser (Statment SynExpr)
assignParser = do
  varName <- varNameParser
  arrayIndices <- many $ do
                   tokenParser LBracketToken
                   ex <- exprParser
                   tokenParser RBracketToken
                   return ex
  let rValue = foldl RValueArrayAccess (RValueVar varName) arrayIndices
  pos <- getPosition
  tokenParser AssignToken
  expr <- exprParser
  return $ AssignSt rValue expr pos

funParser :: TokenParser (Statment SynExpr)
funParser = do
  fun <- funExprParser
  pos <- getPosition
  return $ FunSt fun pos

returnParser :: TokenParser (Statment SynExpr)
returnParser = do
  tokenParser ReturnToken
  pos <- getPosition
  expr <- option VoidSynExpr exprParser
  return $ ReturnSt expr pos
                
-- Expressions 
exprParser :: TokenParser SynExpr
exprParser = do
  expr <- choice [try opExprParser
                ,try numExprParser
                ,try boolExprParser
                ,try timeExprParser
                ,try funExprParser
                ,try varExprParser
                ,try subExprParser
                ,try stringExprParser]
  pos <- getPosition
  arrayIndices <- many $ do
                   tokenParser LBracketToken
                   ex <- exprParser
                   tokenParser RBracketToken
                   return ex
  return $ foldl (ArrayAccessSynExpr pos) expr arrayIndices

funExprParser :: TokenParser SynExpr
funExprParser = do
  (IdentifierToken funName) <- identifierParser
  pos <- getPosition
  tokenParser LParToken
  args <- sepBy exprParser (tokenParser CommaToken)
  tokenParser RParToken
  return $ FunSynExpr (FuncName funName) args pos

numExprParser :: TokenParser SynExpr
numExprParser = do
  pos <- getPosition
  (NumberToken n) <- numberParser
  return $ NumSynExpr n pos

stringExprParser :: TokenParser SynExpr
stringExprParser = do
  (StringToken s) <- stringParser
  return $ StringSynExpr s

boolExprParser :: TokenParser SynExpr
boolExprParser = do
  boolToken <- boolParser
  return $ BoolSynExpr $ case boolToken of
                           TrueToken -> True
                           FalseToken -> False
                           _ -> error "Wut?"

timeExprParser :: TokenParser SynExpr
timeExprParser = do
  (TimeToken time) <- timeParser
  return $ TimeSynExpr time
                           
subExprParser :: TokenParser SynExpr
subExprParser = do
  tokenParser LParToken
  expr <- exprParser
  tokenParser RParToken
  return $ SubSynExpr expr

varExprParser :: TokenParser SynExpr
varExprParser = do
  varName <- varNameParser
  pos <- getPosition
  return $ VarSynExpr varName pos

tokenOps :: [(Token, SynOper)]
tokenOps = [ (PlusToken, PlusSynOp)
           , (MinusToken, MinusSynOp)
           , (AsteriskToken, MulSynOp)
           , (SlashToken, DivSynOp)
           , (EqualToken, EqlSynOp)
           , (NEqualToken, NEqlSynOp)
           , (GrToken, GrSynOp)
           , (LsToken, LsSynOp)
           , (GrEqToken, GrEqSynOp)
           , (LsEqToken, LsEqSynOp)
           , (AndToken, AndSynOp)
           , (OrToken, OrSynOp)
           , (XorToken, XorSynOp)]

opExprParser :: TokenParser SynExpr
opExprParser = do
  lexpr <- choice [try numExprParser
                 ,try boolExprParser
                 ,try timeExprParser
                 ,try funExprParser
                 ,try varExprParser
                 ,try subExprParser
                 ,try stringExprParser]
  op <- choice [try $ tokenParser tok
                   | (tok, _) <- tokenOps]
  pos <- getPosition
  rexpr <- exprParser
  case lookup op tokenOps of
    Nothing -> return $ error "Operator unknown\n"
    Just o -> return $ OpSynExpr o lexpr rexpr pos

varNameParser :: TokenParser VarName
varNameParser = do
  varNameParts <- sepBy1 identifierParser (tokenParser DotToken)
  pos <- getPosition
  let originalVarName = case length varNameParts of
                          1 -> let (IdentifierToken varId) = head varNameParts in
                              VarName varId
                          2 -> let (IdentifierToken thId) = head varNameParts 
                                   (IdentifierToken varId) = varNameParts !! 1 in
                              LongVarName thId varId
                          _ -> throw $ VarNameTooLongSynExc
                              (concat $ map (\(IdentifierToken th) -> th++".") varNameParts) pos
  return $ originalVarName
  
typeNameParser :: TokenParser VarTypeName
typeNameParser = do
  (IdentifierToken typeName) <- identifierParser
  let originalTypeName = VarTypeName typeName
  arraySizes <- many $ do
                   tokenParser LBracketToken
                   (NumberToken n) <- numberParser
                   tokenParser RBracketToken
                   return n
  return $ foldl ArrayVarTypeName originalTypeName arraySizes

-- -- Helper funs
tokenTestParser :: (Token -> Bool) -> TokenParser Token
tokenTestParser test = token showTok posFromTok testTok
    where
      showTok t = show t
      posFromTok Tagged{sourcePos}  = sourcePos
      testTok :: (Tagged Token) -> Maybe Token
      testTok t = if (test.value) t then Just (value t) else Nothing

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

stringParser :: TokenParser Token
stringParser = tokenTestParser(\x -> case x of
                                      (StringToken _) -> True
                                      _ -> False)

boolParser :: TokenParser Token
boolParser = tokenTestParser(\x -> case x of
                                    TrueToken -> True
                                    FalseToken -> True
                                    _ -> False)

timeParser :: TokenParser Token
timeParser = tokenTestParser(\x -> case x of
                                    (TimeToken _) -> True
                                    _ -> False)

zeroLastWait :: TokenParser ()
zeroLastWait = do
  state <- getState
  setState state{parserLastWait=1}

incLastWait :: TokenParser Int
incLastWait = do
  state@ParserState{parserLastWait} <- getState
  setState state{parserLastWait=parserLastWait+1}
  return parserLastWait