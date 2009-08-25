import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Language.Rfx.Compiler() -- I can't test it here
import Language.Rfx.Tokens
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Structures
import Language.Rfx.Util

main :: IO ()
main = do
  defaultMain tests

tests :: [Test.Framework.Test]
tests = [ testGroup "Tauto tests"
          [ testProperty "Tauto property" tautoProp
          , testCase "Tauto test" tautoTest]
        , testGroup "Lexer tests"
          [ testCase "Empty lexer string => empty token list" lexerEmptyTest
          , testProperty "Lexing positive numbers" lexerPosNumProp
          , testProperty "Lexing negative numbers" lexerNegNumProp
          , testCase "Keyword tokens lexing" lexerKeywordsTest
          , testCase "Symbol tokens lexing" lexerSymbolsTest
          , testCase "Case insentivity in lexer" lexerCaseInsensivityTest
          , testCase "Keyword bounaries" lexerMultiKeywordsTest
          , testCase "Complex identifiers" lexerComplexIdentifierTest
          , testCase "Russian utf-8 identifiers" lexerRussianTest]
        , testGroup "Parser tests"
          [ testCase "Thread and state without statments" parserEmptyTest
          , testCase "Multiple threads program" parserMultipleThreadsTest
          , testCase "Assing statment" parserAssignStatmentTest]]

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

lexerComplexIdentifierTest :: Assertion
lexerComplexIdentifierTest = do
  "test1" `lexerAssert` [IdentifierToken "TEST1"]
  "t_e_s_12_" `lexerAssert` [IdentifierToken "T_E_S_12_"]
  "12test12" `lexerAssert` [NumberToken 12, IdentifierToken "TEST12"]
  "-_test_-" `lexerAssert` [MinusToken, IdentifierToken "_TEST_", MinusToken]

lexerRussianTest :: Assertion
lexerRussianTest = do
  "идентифер" `lexerAssert` [IdentifierToken "ИДЕНТИФЕР"]
  "если переменная иначе чо конец" `lexerAssert` [IfToken, IdentifierToken "ПЕРЕМЕННАЯ", ElseToken,
                                                  IdentifierToken "ЧО", EndToken]

-- Parser tests
parserAssert :: String -> Program -> Assertion
parserAssert s p = parseProgram (lexString s) @?= p

parserAssertStatment :: String -> [Statment] -> Assertion
parserAssertStatment s stm = ("thread th where\nstate st where\n" ++ s ++"\nend;\nend;")
                             `parserAssert`
                             Program [Thread "TH" [ThreadState "ST" stm]]

parserEmptyTest :: Assertion
parserEmptyTest = "" `parserAssertStatment` []

parserMultipleThreadsTest :: Assertion
parserMultipleThreadsTest = do
  "поток яЪ где\n \
  \  состояние яЪ где\n \
  \  конец;\n \
  \конец;\n \
  \thread b where\n \
  \  state bb where\n \
  \  end;\n \
  \  state bbb where\n \
  \  end;\n \
  \end;"
  `parserAssert`
  Program [ Thread "ЯЪ" [ThreadState "ЯЪ" []]
          , Thread "B" [ThreadState "BB" [], ThreadState "BBB" []]]
  
parserAssignStatmentTest :: Assertion
parserAssignStatmentTest = do
  "int8 var = 0; var = 12;" `parserAssertStatment` [AssignSt (Var "VAR" (NumExpr 0) (InState "TH" "ST") Int8Type) (NumExpr 12)]
