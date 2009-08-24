module Language.Rfx.Compiler(compileProgram, CompilerTarget(..), CompilerOptions(..),
                            defaultCompilerOptions)
where
import Control.Monad.State
import qualified Data.Map as Map

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
                   , iterators :: Map.Map String Int
                   , indent :: Int
                   } deriving (Eq, Show)

type Compiler a = State CompilerState a

addLine :: String -> Compiler ()
addLine s = State (\state -> ((), state{code=(code state)
                                          ++ (replicate (indent state) ' ')
                                          ++ s
                                          ++ "\n"}))

addString :: String -> Compiler ()
addString s = State (\state -> ((), state{code=(code state)++s}))

makeIndent :: Compiler ()
makeIndent = State (\state -> ((), state{code=(code state)
                                          ++ (replicate (indent state) ' ')}))

nextIterator :: String -> Compiler Int
nextIterator s = State (\state ->
                            let its = iterators state
                                ret = its Map.! s in
                            (ret, state{iterators=
                                           ( Map.update (Just . (+1)) s its)}))

zeroIterator :: String -> Compiler ()
zeroIterator s = State (\state ->
                            let its = iterators state in
                            ((), state{iterators=
                                           (if Map.member s its then
                                                Map.update (const $ Just 0) s its
                                            else
                                                Map.insert s 0 its)}))

addIndent :: Compiler ()
addIndent = State (\state -> ((), state{indent=(indent state)+4}))

subIndent :: Compiler ()
subIndent = State (\state -> ((), state{indent=(indent state)-4}))

defaultCompilerOptions :: CompilerOptions
defaultCompilerOptions = CompilerOptions PIC

compileProgram :: CompilerOptions -> Program -> String
compileProgram op pr = code $ execState (programCompiler pr) $ CompilerState "" op Map.empty 0

programCompiler :: Program -> Compiler ()
programCompiler (Program threads) = do
  zeroIterator "threadN"
  addLine "//Rfx was here"
  addLine $ "#define __RFX_THREAD_COUNT " ++ (show $ length threads)
  addLine $ "enum __rfx_threads {"
  addIndent
  sequence_ [do
              i <- nextIterator "threadN"
              addLine $ "__rfx_thread_"
                            ++ (tlString stateName) ++ " = "
                            ++ (show i) ++ ","
             | Thread stateName _ <- threads]
  subIndent
  addLine $ "};"
  sequence_ [do
              zeroIterator "stateN"
              let thName = tlString $ threadName thread
              addLine $ "enum __rfx_" ++ thName ++ "_states {"
              addIndent
              sequence_ [do
                          i <- nextIterator "stateN"
                          addLine $ "__rfx_state_" ++ thName ++ "_"
                                        ++ (tlString $ stateName state)
                                        ++ " = " ++ (show i) ++ ","
                         | state <- states thread]
              subIndent
              addLine $ "};"
             | thread <- threads]

  sequence_ [do
              let thName = tlString $ threadName thread
              sequence_ [do
                          stateCompiler thName state
                          | state <- states thread]
             | thread <- threads]

stateCompiler :: String -> ThreadState -> Compiler ()
stateCompiler thName state = do
  addLine "\nvoid"
  addLine $ "__rfx_state_" ++ thName ++ "_"
                   ++ (tlString $ stateName state) ++ "_fun()"
  addLine "{"
  addIndent
  sequence_ [do
              statmentCompiler st
             | st <- statments state]
  subIndent
  addLine "}"

statmentCompiler :: Statment -> Compiler ()
statmentCompiler (AssignSt (Variable varName) expr) = do
  makeIndent
  addString $ varName ++ " = "
  exprCompile expr
  addString $ ";\n"

exprCompile :: Expr -> Compiler ()
exprCompile (NumExpr n) = do
  addString $ show n