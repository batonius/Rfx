-- | Rfx main file
import System.Environment(getArgs)
import Language.Rfx.Compiler

main :: IO ()
main = do
  (inFile:_) <- getArgs
  inFileContent <- readFile inFile
  print $ compileFile defaultCompilerOptions inFileContent 