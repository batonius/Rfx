module Language.Rfx.Compiler({-compileProgram, CompilerTarget(..), CompilerOptions(..),
                            defaultCompilerOptions-})
where

import Control.Monad.State
import qualified Data.Map as Map
import Language.Rfx.Compiler.Common
import Language.Rfx.Structures
import qualified Language.Rfx.Compiler.C as C

-- defaultCompilerOptions :: CompilerOptions
-- defaultCompilerOptions = CompilerOptions{compilerLanguage=C, compilerTarget=PIC}

-- compileProgram :: CompilerOptions -> Program -> String
-- compileProgram op pr = let compiler = case compilerLanguage op of
--                                         C -> C.programCompiler
--                                         _ -> C.programCompiler
--                        in
--                          compilerCode $ execState (compiler pr) $ CompilerState "" op Map.empty
--                                           0 (programVars pr) (Thread "" [])
