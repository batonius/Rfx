module Language.Rfx.Parser(parseProgram)
where
import Language.Rfx.Tokens
import Language.Rfx.Structures
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

type TokenParser = GenParser Token ()

parseProgram :: [Token] -> Program
parseProgram ts = case parse programParser "" ts of
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
identifierParser = tokenTestParser (\(IdentifierToken _) -> True)

numberParser :: TokenParser Token
numberParser = tokenTestParser (\(NumberToken _) -> True)

programParser :: TokenParser [Thread]
programParser = many threadParser

threadParser :: TokenParser Thread
threadParser = do
  tokenParser ThreadToken
  (IdentifierToken thName) <- identifierParser
  tokenParser WhereToken
  sts <- manyTill stateParser $ do
                 tokenParser EndToken
                 tokenParser SemicolonToken
  return $ Thread thName sts

stateParser :: TokenParser ThreadState
stateParser = do
  tokenParser StateToken
  (IdentifierToken stName) <- identifierParser
  tokenParser WhereToken
  sts <- manyTill statmentParser $ do
                tokenParser EndToken
                tokenParser SemicolonToken
  return $ ThreadState stName sts

statmentParser :: TokenParser Statment
statmentParser = do
  statment <- choice [try assignParser]
  tokenParser SemicolonToken
  return statment

assignParser :: TokenParser Statment
assignParser = do
  (IdentifierToken varName) <- identifierParser
  tokenParser AssignToken
  expr <- exprParser
  return $ AssignSt (Variable varName) expr

exprParser :: TokenParser Expr
exprParser = do
  (NumberToken n) <- numberParser
  return $ NumExpr n
