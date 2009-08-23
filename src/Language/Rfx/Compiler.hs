module Language.Rfx.Compiler(compileFile, CompilerTarget(..), CompilerOptions(..),
                            defaultCompilerOptions)
where
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Structures
  
data CompilerTarget = PIC | ATM | DOS  
data CompilerOptions = CompilerOptions
                     {
                       target :: CompilerTarget
                       -- Options goes here
                     }

defaultCompilerOptions :: CompilerOptions
defaultCompilerOptions = CompilerOptions PIC
                     
compileFile :: CompilerOptions -> String -> String
compileFile _ s = compileProgram $ parseProgram $ lexString s

compileProgram :: Program -> String
compileProgram (Program []) = ""
compileProgram _ = undefined