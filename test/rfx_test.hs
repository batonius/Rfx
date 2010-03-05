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
    
import Language.Rfx.Compiler() -- I can't test it here
import Language.Rfx.Tokens
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Structures
import Language.Rfx.Util

main :: IO ()
main = do
  goodFiles <- getDirectoryContents "./test/succ"
  badFiles <- getDirectoryContents "./test/fail"
  let rfxFiles = filter (List.isSuffixOf ".rfx")
  defaultMain $ tests (rfxFiles goodFiles) (rfxFiles badFiles)

tests :: [String] -> [String] -> [Test.Framework.Test]
tests goodFiles badFiles = [ testGroup "Tauto tests"
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
                             , testCase "Russian utf-8 identifiers" lexerRussianTest
                             , testCase "Strings lexing" lexerStringTest]
                           , testGroup "Parser tests"
                             [ testCase "Thread and state without statments" parserEmptyTest
                             , testCase "Multiple threads program" parserMultipleThreadsTest
                             , testCase "Assing statment" parserAssignStatmentTest
                             , testCase "Expression parsing" parserExprTest]
                           , testGroup "Success file test"
                             [ testCase ("File " ++ file) $ compileFileSuccessAssertion ("./test/succ/" ++ file)
                               | file <- goodFiles ]
                           , testGroup "Fail file test"
                             [ testCase ("File " ++ file) $ compileFileFailAssertion ("./test/fail/" ++ file)
                                   | file <- badFiles]
                           ]

tautoProp :: Int -> Property
tautoProp x = odd x ==>
              odd (x^(2::Int))

tautoTest :: Assertion
tautoTest = (1::Int) @?= (1::Int)

-- Lexer tests
lexerAssert :: String -> [Token] -> Assertion
lexerAssert s t = (map value $ lexString s) @?= t

lexerEmptyTest :: Assertion
lexerEmptyTest = "" `lexerAssert` []

lexerPosNumProp :: Int -> Property
lexerPosNumProp n = n>=0 ==> (map value $ lexString (show n)) == [NumberToken n]

lexerNegNumProp :: Int -> Property
lexerNegNumProp n = n<0 ==> (map value $ lexString (show n)) == [MinusToken, NumberToken (abs n)]

lexerKeywordsTest :: Assertion
lexerKeywordsTest = sequence_ [s `lexerAssert` [t] | (s, t) <- keywordTokens]

lexerSymbolsTest :: Assertion
lexerSymbolsTest = sequence_ [s `lexerAssert` [t] | (s, t) <- symbolTokens]

lexerCaseInsensivityTest :: Assertion
lexerCaseInsensivityTest = do
  " iF\n" `lexerAssert` [IfToken]
  "\tthReaD\n\f" `lexerAssert` [ThreadToken]
  "  \t  \n someIdenTifier\n \f" `lexerAssert` [IdentifierToken "someIdenTifier"]

lexerMultiKeywordsTest :: Assertion
lexerMultiKeywordsTest = do
  "if-thread" `lexerAssert` [IfToken, MinusToken, ThreadToken]
  "ifthread" `lexerAssert` [IdentifierToken "ifthread"]
  "if(thread)+state\tend" `lexerAssert` [IfToken, LParToken, ThreadToken, RParToken, PlusToken, StateToken, EndToken]
  "if\nthread\nstate" `lexerAssert` [IfToken, ThreadToken, StateToken]

lexerComplexIdentifierTest :: Assertion
lexerComplexIdentifierTest = do
  "test1" `lexerAssert` [IdentifierToken "test1"]
  "t_e_s_12_" `lexerAssert` [IdentifierToken "t_e_s_12_"]
  "12test12" `lexerAssert` [NumberToken 12, IdentifierToken "test12"]
  "-_tEst_-" `lexerAssert` [MinusToken, IdentifierToken "_tEst_", MinusToken]

lexerRussianTest :: Assertion
lexerRussianTest = do
  "идентифер" `lexerAssert` [IdentifierToken "идентифер"]
  "если переменная иначе чо конец" `lexerAssert` [IfToken, IdentifierToken "переменная", ElseToken,
                                                  IdentifierToken "чо", EndToken]
lexerStringTest :: Assertion
lexerStringTest = do
  "\"12\"12+" `lexerAssert` [StringToken "12", NumberToken 12, PlusToken]
  "31\"_asf123'xzcv,.<>\"-" `lexerAssert` [NumberToken 31, StringToken "_asf123'xzcv,.<>", MinusToken]
  
-- Parser tests
parserAssert :: String -> Program SynExpr -> Assertion
parserAssert s p = (parseProgram $ lexString s) @?= p

varForTest :: Var SynExpr
varForTest = Var{varName="var"
                ,varInitValue=(NumSynExpr 0)
                ,varScope=InGlobal
                ,varType=Int8Type}
             
varNameForTest :: VarName
varNameForTest = VarName "var"
                   
parserAssertStatment :: String -> [Statment SynExpr] -> Assertion
parserAssertStatment s stm = ("int8 var = 0;thread th where\nstate st where\n" ++ s ++"\nend;\nend;")
                             `parserAssert`
                             Program [Thread "th" [ThreadState "st" stm]]
                                         [varForTest]

parserAssertExpr :: String -> SynExpr -> Assertion
parserAssertExpr s e = ("var = " ++ s ++ ";")
                       `parserAssertStatment`
                       [AssignSt varNameForTest e]
                                         
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
  Program [ Thread "яЪ" [ThreadState "яЪ" []]
          , Thread "b" [ThreadState "bb" [], ThreadState "bbb" []]] []
  
parserAssignStatmentTest :: Assertion
parserAssignStatmentTest = do
  "var = 12;" `parserAssertStatment` [AssignSt varNameForTest (NumSynExpr 12)]

parserExprTest :: Assertion
parserExprTest = do
  "10" `parserAssertExpr` (NumSynExpr 10)
  "10+3"  `parserAssertExpr` (OpSynExpr PlusSynOp (NumSynExpr 10) (NumSynExpr 3))
  "var" `parserAssertExpr` (VarSynExpr varNameForTest)
  "(1*1)-var" `parserAssertExpr` (OpSynExpr MinusSynOp
                                  (SubSynExpr $ OpSynExpr MulSynOp (NumSynExpr 1) (NumSynExpr 1))
                                  (VarSynExpr varNameForTest))

-- --
compileFileSuccessAssertion :: FilePath -> Assertion
compileFileSuccessAssertion file = do
  curDir <- getCurrentDirectory
  ec <- system (curDir++"/rfx " ++ file ++ " > out.c")
  ec @?= ExitSuccess
  ec2 <- system ("gcc -fsyntax-only -std=c89 -c out.c")
  ec2 @?= ExitSuccess
      
compileFileFailAssertion :: FilePath -> Assertion
compileFileFailAssertion file = do
  curDir <- getCurrentDirectory
  ec <- system (curDir++"/rfx " ++ file ++ " > out.c")
  assertBool "Rfx failed" (ec /= ExitSuccess)
