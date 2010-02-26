-- | Rfx main file
import System.Environment(getArgs)
import Prelude(($), IO, map, show)
import System.IO.UTF8
-- TODO remove unnessesary
-- import Language.Rfx.Compiler
import Language.Rfx.Lexer
import Language.Rfx.Parser

main :: IO ()
main = do
  (inFile:_) <- getArgs
  inFileString <- readFile inFile
  -- let lexs = lexString inFileString
  -- print lexs
  -- let parse = parseProgram lexs
  -- print parse
  -- writeFile "out.c" $
  putStrLn $ show $
           --compileProgram defaultCompilerOptions $
           parseProgram $ map value $ lexString inFileString
