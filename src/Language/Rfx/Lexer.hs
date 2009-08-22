module Language.Rfx.Lexer(lexString)
where
import Language.Rfx.Tokens
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.Char(toUpper)

lexString :: String -> [Token]
lexString s = case parse mainLexer "" (map toUpper s) of
                Left err -> error $ "Lexer error\n" ++ show err
                Right ts -> ts

-- | Parses a single whitespace character.
whiteSpaceCharParser :: Parser Char
whiteSpaceCharParser = oneOf " \v\f\t\n" <?> "SPACE"

-- | Parses a stretch of whitespace.
whiteSpaceParser :: Parser ()
whiteSpaceParser = skipMany (whiteSpaceCharParser <?> "")

-- | Parses a predefined string
keywordParser :: String -> Parser String
keywordParser s = string s <?> ("keyword " ++ s)

-- | Parses number
numberParser :: Parser Token
numberParser = do
  whiteSpaceParser
  numberString <- many1 digit
  return $ NumberToken (read numberString)

-- | Parse indentifier
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
         
tokenParser :: Parser Token
tokenParser = choice $ [try numberParser] ++ [try $ do
                                                whiteSpaceParser
                                                keywordParser s
                                                return t
                                              | (s, t) <- tokenStrings]
           ++ [try identifierParser]

mainLexer :: Parser [Token]
mainLexer = manyTill tokenParser $ try eofParser