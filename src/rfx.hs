{-# LANGUAGE ScopedTypeVariables #-}
-- | Rfx main file
import System.Environment(getArgs)
import System.Exit
import Prelude(($), IO, map, show, (++))
import System.IO.UTF8
-- TODO remove unnessesary
import Language.Rfx.Compiler
import Language.Rfx.Validator
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Error
import Control.Exception

compileFile inFile = do
  putStrLn $
           compileProgram defaultCompilerOptions $ validateProgram $ 
           parseProgram $ lexString inFile
            
main :: IO ()
main = do
  (inFile:_) <- getArgs
  inFileString <- readFile inFile
  (compileFile inFileString) `catch`
       (\ (ex::RfxException) -> do
          putStrLn $ "Lol i catch: " ++ (show ex)
          exitWith $ ExitFailure 1)
