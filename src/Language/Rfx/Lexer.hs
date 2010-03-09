{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Lexer (lexString, Tagged(..))
where
import Language.Rfx.Tokens
import Language.Rfx.Error
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Exception hiding (try)
import Data.Char(toUpper, toLower)

lexString :: String -> [Tagged Token]
lexString s = case parse mainLexer "" s of
                Left err -> throw $ LexException err
                Right ts -> filter (\Tagged{value}->value/=CommentToken) ts

taggedParser :: Parser a -> Parser (Tagged a)
taggedParser parser = liftM2 Tagged getPosition parser

whiteSpaceCharLexer :: Parser Char
whiteSpaceCharLexer = oneOf " \v\f\t\n" <?> "SPACE"

whiteSpaceLexer :: Parser ()
whiteSpaceLexer = skipMany (whiteSpaceCharLexer <?> "")

lineCommentLexer :: Parser (Tagged Token)
lineCommentLexer = taggedParser $ do
  whiteSpaceLexer
  symbolLexer "//"
  manyTill anyChar $ try (char '\n')
  return CommentToken

multiLineCommentLexer :: Parser (Tagged Token)
multiLineCommentLexer = taggedParser $ do
  whiteSpaceLexer
  symbolLexer "(*"
  manyTill anyChar $ try (symbolLexer "*)")
  return CommentToken
         
anyCaseStringParser :: String -> Parser String
anyCaseStringParser s = sequence [(char $ toLower c) <|> (char $ toUpper c)
                                  | c <- s]

keywordLexer :: String -> Parser (Tagged String)
keywordLexer str = (taggedParser $ do
                     st <- anyCaseStringParser str
                     lookAhead $ choice
                               $ ([do try whiteSpaceCharLexer; return ""]
                                  ++ [do try eof; return ""]
                                  ++ [try $ string s | (s, _) <- symbolTokens])
                     return st) <?> ("keyword " ++ str)

symbolLexer :: String -> Parser (Tagged String)
symbolLexer s = (taggedParser $ string s) <?> ("symbol " ++ s)

numberLexer :: Parser (Tagged Token)
numberLexer = taggedParser $ do
  whiteSpaceLexer
  numberString <- many1 digit
  return $ NumberToken (read numberString)

stringLexer :: Parser (Tagged Token)
stringLexer = taggedParser $ do
  whiteSpaceLexer
  char '\"'
  string <- manyTill anyChar $ try $ char '\"'
  return $ StringToken string

identifierLexer :: Parser (Tagged Token)
identifierLexer = taggedParser $ do
  whiteSpaceLexer
  firstChar <- (letter <|> char '_')
  idString <- many $ letter <|> digit <|> char '_'
  return $ IdentifierToken (firstChar:idString)

eofLexer :: Parser (Tagged Token)
eofLexer = taggedParser $ do
  whiteSpaceLexer
  eof
  return $ EOFToken

symbolLexers :: [Parser (Tagged Token)]
symbolLexers = [try $  do
                 whiteSpaceLexer
                 pos <- getPosition
                 symbolLexer s
                 return (Tagged pos t)
                | (s, t) <- symbolTokens]

keywordLexers :: [Parser (Tagged Token)]
keywordLexers = [try $ do
                  whiteSpaceLexer
                  pos <- getPosition
                  keywordLexer s
                  return (Tagged pos t)
                 | (s, t) <- keywordTokens]

tokenLexer :: Parser (Tagged Token)
tokenLexer = choice $ [try multiLineCommentLexer
                      ,try lineCommentLexer
                      ,try stringLexer
                      ,try numberLexer]
              ++ keywordLexers
              ++ symbolLexers
              ++ [try identifierLexer]

mainLexer :: Parser [Tagged Token]
mainLexer = manyTill tokenLexer $ try eofLexer