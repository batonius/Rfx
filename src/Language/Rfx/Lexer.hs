module Language.Rfx.Lexer(lexString)
where
import Language.Rfx.Tokens
import Text.ParserCombinators.Parsec
import Data.Char(toUpper)

lexString :: String -> [Token]
lexString s = case parse mainLexer "" (map toUpper s) of
                Left err -> error $ "Error\n" ++ show err
                Right ts -> ts

whiteSpaceCharLexer :: Parser Char
whiteSpaceCharLexer = oneOf " \v\f\t\n" <?> "SPACE"

whiteSpaceLexer :: Parser ()
whiteSpaceLexer = skipMany (whiteSpaceCharLexer <?> "")

keywordLexer :: String -> Parser String
keywordLexer str = (do
                    st <- string str
                    lookAhead $ choice $ ([do try whiteSpaceCharLexer; return ""]
                                          ++ [do try eof; return ""]
                                          ++ [try $ string s | (s, _) <- symbolTokens])
                    return st) <?> ("keyword " ++ str)

symbolLexer :: String -> Parser String
symbolLexer s = string s <?> ("symbol " ++ s)

numberLexer :: Parser Token
numberLexer = do
  whiteSpaceLexer
  numberString <- many1 digit
  return $ NumberToken (read numberString)

identifierLexer :: Parser Token
identifierLexer = do
  whiteSpaceLexer
  firstChar <- (letter <|> char '_')
  idString <- many $ letter <|> digit <|> char '_'
  return $ IdentifierToken (firstChar:idString)

eofLexer :: Parser Token
eofLexer = do
  whiteSpaceLexer
  eof
  return $ EOFToken

symbolLexers :: [Parser Token]
symbolLexers = [try $ do
                   whiteSpaceLexer
                   symbolLexer s
                   return t
                 | (s, t) <- symbolTokens]

keywordLexers :: [Parser Token]
keywordLexers = [try $ do
                    whiteSpaceLexer
                    keywordLexer s
                    return t
                  | (s, t) <- keywordTokens]

tokenLexer :: Parser Token
tokenLexer = choice $ [try numberLexer]
              ++ keywordLexers
              ++ symbolLexers
              ++ [try identifierLexer]

mainLexer :: Parser [Token]
mainLexer = manyTill tokenLexer $ try eofLexer