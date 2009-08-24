module Language.Rfx.Compiler(compileProgram, CompilerTarget(..), CompilerOptions(..),
                            defaultCompilerOptions)
where
import Control.Monad.State
  
import Language.Rfx.Structures
import Language.Rfx.Util
  
data CompilerTarget = PIC | ATM | DOS deriving (Eq, Show)
data CompilerOptions = CompilerOptions
                     {
                       target :: CompilerTarget
                       -- Options goes here
                     } deriving (Eq, Show)

data CompilerState = CompilerState
                   {
                     code :: String
                   , options :: CompilerOptions
                   } deriving (Eq, Show)
                     
type Compiler a = State CompilerState a

addString :: String -> Compiler ()
addString s = State (\state -> ((), state{code=(code state)++s}))
    
defaultCompilerOptions :: CompilerOptions
defaultCompilerOptions = CompilerOptions PIC

compileProgram :: CompilerOptions -> Program -> String
compileProgram op pr = code $ execState (programCompiler pr) $ CompilerState "" op

programCompiler :: Program -> Compiler ()
programCompiler (Program threads) = do
  addString "//Rfx was here\n"
  addString $ "#define __RFX_THREAD_COUNT " ++ (show $ length threads) ++ "\n"
  addString $ "enum __rfx_threads {\n"
  sequence_ [addString $ "  __rfx_thread_" ++ (tlString stateName) ++ ",\n"
             | Thread stateName _ <- threads]
  addString $ "};"