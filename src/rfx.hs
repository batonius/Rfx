-- | Rfx main file
import System.Environment(getArgs)

-- TODO remove unnessesary    
import Language.Rfx.Compiler
import Language.Rfx.Tokens
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Structures

main :: IO ()
main = do
  (inFile:_) <- getArgs
  inFileContent <- readFile inFile
  print $ compileFile defaultCompilerOptions inFileContent 