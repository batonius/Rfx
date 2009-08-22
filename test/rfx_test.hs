import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Language.Rfx.Compiler
import Language.Rfx.Tokens
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Structures

main :: IO ()
main = do
  defaultMain tests

tests :: [Test.Framework.Test]
tests = [ testGroup "Tauto tests"
          [ testProperty "Tauto property" tautoProp
          , testCase "Tauto test" tautoTest]
          ,
          testGroup "Lexer tests"
          [ testCase "Empty lexer string => empty token list" lexerEmptyTest
          , testProperty "Lexing positive numbers" lexerPosNumProp
          , testProperty "Lexing negative numbers" lexerNegNumProp
          , testCase "Keyword tokens lexing" lexerKeywordsTest
          , testCase "Symbol tokens lexing" lexerSymbolsTest
          , testCase "Case insentivity in lexer" lexerCaseInsensivityTest
          , testCase "Keyword bounaries" lexerMultiKeywordsTest]]


tautoProp :: Int -> Property
tautoProp x = odd x ==>
              odd (x^(2::Int))

tautoTest :: Assertion
tautoTest = (1::Int) @?= (1::Int)

-- Lexer tests
lexerAssert :: String -> [Token] -> Assertion
lexerAssert s t = lexString s @?= t

lexerEmptyTest :: Assertion
lexerEmptyTest = "" `lexerAssert` []

lexerPosNumProp :: Int -> Property
lexerPosNumProp n = n>=0 ==> lexString (show n) == [NumberToken n]

lexerNegNumProp :: Int -> Property
lexerNegNumProp n = n<0 ==> lexString (show n) == [MinusToken, NumberToken (abs n)]

lexerKeywordsTest :: Assertion
lexerKeywordsTest = sequence_ [s `lexerAssert` [t] | (s, t) <- keywordTokens]

lexerSymbolsTest :: Assertion
lexerSymbolsTest = sequence_ [s `lexerAssert` [t] | (s, t) <- symbolTokens]

lexerCaseInsensivityTest :: Assertion
lexerCaseInsensivityTest = do
  " iF\n" `lexerAssert` [IfToken]
  "\tthReaD\n\f" `lexerAssert` [ThreadToken]
  "  \t  \n someIdenTifier\n \f" `lexerAssert` [IdentifierToken "SOMEIDENTIFIER"]

lexerMultiKeywordsTest :: Assertion
lexerMultiKeywordsTest = do
  "if-thread" `lexerAssert` [IfToken, MinusToken, ThreadToken]
  "ifthread" `lexerAssert` [IdentifierToken "IFTHREAD"]
  "if(thread)+state\tend" `lexerAssert` [IfToken, LParToken, ThreadToken, RParToken, PlusToken, StateToken, EndToken]
  "if\nthread\nstate" `lexerAssert` [IfToken, ThreadToken, StateToken]
