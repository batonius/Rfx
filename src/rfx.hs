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
import Control.Exception

compileFile :: String -> IO ()
compileFile inFile = do
  putStrLn $
           compileProgram defaultCompilerOptions $
           validateProgram $ 
           parseProgram $
           lexString inFile
  return ()

main :: IO ()
main = do
  (inFile:_) <- getArgs
  inFileString <- readFile inFile
  (compileFile inFileString) `catch`
       (\ (ex::RfxException) -> do
          putStrLn $ "Catched: " ++ (show ex)
          exitWith $ ExitFailure 1)
