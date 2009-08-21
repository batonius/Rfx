import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Language.Rfx.Compiler
import Language.Rfx.Tokens()
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Structures
    
main :: IO ()    
main = do
  defaultMain tests

tests :: [Test.Framework.Test]              
tests = [ testGroup "rfx tests" [
                testProperty "Tauto property" tautoProp,
                testCase "Tauto test" tautoTest,
                testProperty "Non empty compiler input ==> no empty output" emptyProperty,
                testCase "Empty compiler input ==> empty output" emptyTest,
                testCase "Empty lexer string => empty token list" emptyLexerTest,
                testCase "Empty parser list => empty program" emptyParserTest]]

tautoProp :: Int -> Property        
tautoProp x = odd x ==>
              odd (x^(2::Int))

tautoTest :: Assertion              
tautoTest = (1::Int) @?= (1::Int)

emptyTest :: Assertion
emptyTest = compileFile defaultCompilerOptions "" @?= ""

emptyProperty :: String -> Property
emptyProperty s = s /= "" ==>
                  compileFile defaultCompilerOptions s /= ""

emptyLexerTest :: Assertion
emptyLexerTest = (lexString "") @?= []

emptyParserTest :: Assertion
emptyParserTest = (parseTokens []) @?= Program []