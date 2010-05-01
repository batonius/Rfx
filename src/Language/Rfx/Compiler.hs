module Language.Rfx.Compiler(compileProgram, CompilerTarget(..), CompilerOptions(..),
                            defaultCompilerOptions)
where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Rfx.Compiler.Common
import Language.Rfx.Structures
import qualified Language.Rfx.Compiler.C as C

defaultCompilerOptions :: CompilerOptions
defaultCompilerOptions = CompilerOptions{compilerLanguage=C, compilerTarget=PIC}

compileProgram :: CompilerOptions -> Program SemExpr -> String
compileProgram op pr = let compiler = case compilerLanguage op of
                                        C -> C.programCompiler
                                        _ -> C.programCompiler
                       in
                         compilerCode $ execState (compiler pr) $ CompilerState "" op Map.empty
                                          0 (Set.fromList (programVars pr)) (Thread "" []) (ThreadState "" [])
