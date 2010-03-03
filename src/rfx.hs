-- | Rfx main file
import System.Environment(getArgs)
import Prelude(($), IO, map, show)
import System.IO.UTF8
-- TODO remove unnessesary
import Language.Rfx.Compiler
import Language.Rfx.Validator
import Language.Rfx.Lexer
import Language.Rfx.Parser

prog = "int8 x = 0;thread a where state aa where x = 10 + (12 * 10); end; end;"
lexed = lexString prog
parsed = parseProgram $ map value $ lexed
validated = validateProgram parsed
    
main :: IO ()
main = do
  (inFile:_) <- getArgs
  inFileString <- readFile inFile
  putStrLn $
           compileProgram defaultCompilerOptions $ validateProgram $ 
           parseProgram $ map value $ lexString inFileString
