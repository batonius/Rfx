import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.List as List
import System.Directory
import System.Cmd
import System.Exit
import System.IO.Unsafe
    
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
          , testCase "Assing statment" parserAssignStatmentTest
          , testCase "Expression parsing" parserExprTest]
        , testGroup "Success file test"
          [ testCase ("File " ++ file) $ compileFileSuccessAssertion ("./test/succ/" ++ file)
            | file <- filter (List.isSuffixOf ".rfx") $ unsafePerformIO $ getDirectoryContents "./test/succ"
          ]
        , testGroup "Fail file test"
          [ testCase ("File " ++ file) $ compileFileFailAssertion ("./test/fail/" ++ file)
            | file <- filter (List.isSuffixOf ".rfx") $ unsafePerformIO $ getDirectoryContents "./test/fail"
          ]] -- ^ OMG How should i do it?

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

varForTest :: Var
varForTest = (Var "VAR" (NumExpr 0) InGlobal Int8Type)
                   
parserAssertStatment :: String -> [Statment] -> Assertion
parserAssertStatment s stm = ("int8 var = 0;thread th where\nstate st where\n" ++ s ++"\nend;\nend;")
                             `parserAssert`
                             Program [Thread "TH" [ThreadState "ST" stm]]
                                         (Set.insert varForTest Set.empty)

parserAssertExpr :: String -> Expr -> Assertion
parserAssertExpr s e = ("var = " ++ s ++ ";")
                       `parserAssertStatment`
                       [AssignSt varForTest e]
                                         
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
          , Thread "B" [ThreadState "BB" [], ThreadState "BBB" []]] Set.empty
  
parserAssignStatmentTest :: Assertion
parserAssignStatmentTest = do
  "var = 12;" `parserAssertStatment` [AssignSt varForTest (NumExpr 12)]

parserExprTest :: Assertion
parserExprTest = do
  "10" `parserAssertExpr` (NumExpr 10)
  "10+3"  `parserAssertExpr` (OpExpr PlusOp (NumExpr 10) (NumExpr 3))
  "var" `parserAssertExpr` (VarExpr varForTest)
  "(1*1)-var" `parserAssertExpr` (OpExpr MinusOp
                                  (SubExpr $ OpExpr MulOp (NumExpr 1) (NumExpr 1))
                                  (VarExpr varForTest))

--
compileFileSuccessAssertion :: FilePath -> Assertion
compileFileSuccessAssertion file = do
  curDir <- getCurrentDirectory
  ec <- system (curDir++"/rfx " ++ file ++ " > out.c")
  ec @?= ExitSuccess
  ec2 <- system ("gcc -fsyntax-only -c out.c")
  ec2 @?= ExitSuccess
      
compileFileFailAssertion :: FilePath -> Assertion
compileFileFailAssertion file = do
  curDir <- getCurrentDirectory
  ec <- system (curDir++"/rfx " ++ file ++ " > out.c")
  assertBool "Rfx failed" (ec /= ExitSuccess)
