{-# LANGUAGE ScopedTypeVariables #-}
-- | Rfx main file
import System.Environment(getArgs)
import System.Exit
import Prelude hiding(readFile, catch)
import System.IO.UTF8 hiding(putStrLn)
-- TODO remove unnessesary
import Language.Rfx.Compiler
import Language.Rfx.Validator
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Error
import System.Locale.SetLocale
import Text.I18N.GetText
import Control.Exception

compileFile inFile = do
  putStrLn $
           compileProgram defaultCompilerOptions $ validateProgram $ 
           parseProgram $ lexString inFile
  return ()
            
main :: IO ()
main = do
  setLocale LC_ALL (Just "") 
  bindTextDomain "rfx" (Just ".")
  textDomain (Just "rfx")
  (inFile:_) <- getArgs
  inFileString <- readFile inFile
  (compileFile inFileString) `catch`
       (\ (ex::RfxException) -> do
          putStrLn $ "Lol i catch: " ++ (show ex)
          exitWith $ ExitFailure 1)
