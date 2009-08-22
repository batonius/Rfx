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
          , testCase "Basic tokens lexing" lexerTokenStringsTest
          , testCase "Case insentivity in lexer" lexerCaseInsensivityTest]]
            

tautoProp :: Int -> Property        
tautoProp x = odd x ==>
              odd (x^(2::Int))

tautoTest :: Assertion              
tautoTest = (1::Int) @?= (1::Int)

-- Lexer tests            
lexerEmptyTest :: Assertion
lexerEmptyTest = (lexString "") @?= []

lexerPosNumProp :: Int -> Property
lexerPosNumProp n = n>=0 ==> lexString (show n) == [NumberToken n]
                    
lexerNegNumProp :: Int -> Property
lexerNegNumProp n = n<0 ==> lexString (show n) == [NumberToken n]                    

lexerTokenStringsTest :: Assertion
lexerTokenStringsTest = assertBool "Basic tokens lexing" $ all id [[t] == (lexString s) | (s, t) <- tokenStrings]

lexerCaseInsensivityTest :: Assertion
lexerCaseInsensivityTest = do
  lexString " iF\n" @?= [IfToken]
  lexString "\tthReaD\n\f" @?= [ThreadToken]
  lexString "  \t  \n someIdenTifier\n \f" @?= [IdentifierToken "SOMEIDENTIFIER"]
