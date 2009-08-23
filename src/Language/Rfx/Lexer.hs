module Language.Rfx.Lexer(lexString)
where
import Language.Rfx.Tokens
import Text.ParserCombinators.Parsec
import Data.Char(toUpper)

lexString :: String -> [Token]
lexString s = case parse mainLexer "" (map toUpper s) of
                Left err -> error $ "Lexer error\n" ++ show err
                Right ts -> ts

whiteSpaceCharParser :: Parser Char
whiteSpaceCharParser = oneOf " \v\f\t\n" <?> "SPACE"

whiteSpaceParser :: Parser ()
whiteSpaceParser = skipMany (whiteSpaceCharParser <?> "")

keywordParser :: String -> Parser String
keywordParser str = (do
                    st <- string str
                    lookAhead $ choice $ ([do try whiteSpaceCharParser; return ""]
                                          ++ [do try eof; return ""]
                                          ++ [try $ string s | (s, _) <- symbolTokens])
                    return st) <?> ("keyword " ++ str)

symbolParser :: String -> Parser String
symbolParser s = string s <?> ("symbol " ++ s)

numberParser :: Parser Token
numberParser = do
  whiteSpaceParser
  numberString <- many1 digit
  return $ NumberToken (read numberString)

-- TODO Do it right
identifierParser :: Parser Token
identifierParser = do
  whiteSpaceParser
  idString <- many1 letter
  return $ IdentifierToken idString

eofParser :: Parser Token
eofParser = do
  whiteSpaceParser
  eof
  return $ EOFToken

symbolParsers :: [Parser Token]
symbolParsers = [try $ do
                   whiteSpaceParser
                   symbolParser s
                   return t
                 | (s, t) <- symbolTokens]

keywordParsers :: [Parser Token]
keywordParsers = [try $ do
                    whiteSpaceParser
                    keywordParser s
                    return t
                  | (s, t) <- keywordTokens]

tokenParser :: Parser Token
tokenParser = choice $ [try numberParser]
              ++ keywordParsers
              ++ symbolParsers
              ++ [try identifierParser]

mainLexer :: Parser [Token]
mainLexer = manyTill tokenParser $ try eofParser