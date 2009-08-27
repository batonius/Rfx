module Language.Rfx.Compiler(compileProgram, CompilerTarget(..), CompilerOptions(..),
                            defaultCompilerOptions)
where
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
    
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
                     compilerCode :: String
                   , compilerOptions :: CompilerOptions
                   , compilerIterators :: Map.Map String Int
                   , compilerIndent :: Int
                   , compilerVars :: Set.Set Var
                   } deriving (Eq, Show)

type Compiler a = State CompilerState a

defaultCompilerOptions :: CompilerOptions
defaultCompilerOptions = CompilerOptions PIC

compileProgram :: CompilerOptions -> Program -> String
compileProgram op pr@(Program _ vars) = compilerCode $ execState (programCompiler pr) $ CompilerState "" op Map.empty 0 vars

programCompiler :: Program -> Compiler ()
programCompiler (Program threads _) = do
  addLine "//Rfx was here"
  addLine $ "#define __RFX_THREAD_COUNT " ++ (show $ length threads)
  addLine $ "enum __rfx_threads {"
  addIndent
  zeroIterator "threadN"
  sequence_ [do
              i <- nextIterator "threadN"
              addLine $ "__rfx_thread_"
                            ++ (tlString (threadName thread)) ++ " = "
                            ++ (show i) ++ ","
             | thread <- threads]
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
  -- Global vars             
  globalVars <- getVars InGlobal
  sequence_ [varDefenitionCompiler var | var <- globalVars]
  -- Thread vars
  sequence_ [do
              threadVars <- getVars $ InThread (threadName thread)
              sequence_ [ varDefenitionCompiler var
                              | var <- threadVars]
                  | thread <- threads]
  --States          
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
  stateVars <- getVars $ InState thName (stateName state)
  sequence_ [varDefenitionCompiler var | var <- stateVars]
  sequence_ [do
              statmentCompiler st
             | st <- statments state]
  subIndent
  addLine "}"

statmentCompiler :: Statment -> Compiler ()
statmentCompiler (AssignSt var expr) = do
  makeIndent
  addString $ (getVarFullName var) ++ " = "
  exprCompiler expr
  addString $ ";\n"

statmentCompiler _ = error "Not implemented yet"
                     
exprCompiler :: Expr -> Compiler ()
exprCompiler expr = case expr of
                      (NumExpr n) -> addString $ (show n) ++ " "
                      (SubExpr e) -> do
                                addString "( "
                                exprCompiler e
                                addString ") "
                      (VarExpr v) -> addString $ (getVarFullName v) ++ " "
                      (OpExpr op le re) -> do
                                exprCompiler le
                                addString $ case op of
                                              PlusOp  -> "+ "
                                              MinusOp -> "- "
                                              MulOp   -> "* "
                                              DivOp   -> "/ "
                                              _       -> ""
                                exprCompiler re

typeCompiler :: VarType -> Compiler ()
typeCompiler tp = addString $ case tp of
                                _ -> "int"

varDefenitionCompiler :: Var -> Compiler ()
varDefenitionCompiler var = do
  makeIndent
  typeCompiler (varType var)
  addString " "
  addString $ getVarFullName var
  addString " = "
  exprCompiler (varInitValue var)
  addString ";\n"
  
-- State funs

addString :: String -> Compiler ()
addString s = State (\state -> ((), state{compilerCode=(compilerCode state)++s}))

makeIndent :: Compiler ()
makeIndent = State (\state -> ((), state{compilerCode=(compilerCode state)
                                          ++ (replicate (compilerIndent state) ' ')}))

addLine :: String -> Compiler ()
addLine s = do
  makeIndent
  addString s
  addString "\n"

nextIterator :: String -> Compiler Int
nextIterator s = State (\state ->
                            let its = compilerIterators state in
                            if Map.member s its then
                                (its Map.! s, state{compilerIterators=
                                                        Map.update (Just . (+1)) s its})
                            else
                                (0, state{compilerIterators=
                                              Map.insert s 0 its}))
                                         
zeroIterator :: String -> Compiler ()
zeroIterator s = State (\state ->
                            let its = compilerIterators state in
                            ((), state{compilerIterators=
                                           (if Map.member s its then
                                                Map.update (const $ Just 0) s its
                                            else
                                                Map.insert s 0 its)}))

addIndent :: Compiler ()
addIndent = State (\state -> ((), state{compilerIndent=(compilerIndent state)+4}))

subIndent :: Compiler ()
subIndent = State (\state -> ((), state{compilerIndent=(compilerIndent state)-4}))

getVarFullName :: Var -> String
getVarFullName var = "__rfx_" ++ (case (varScope var) of
                                    InGlobal -> ""
                                    InThread tn -> (tlString tn) ++ "_"
                                    InState tn sn -> (tlString tn) ++ "_"
                                                    ++ (tlString sn) ++ "_")
                     ++ (tlString (varName var))

getVars :: ProgramPos -> Compiler [Var]
getVars pos = State(\state -> 
                    (Set.toList $ Set.filter (\ var -> (varScope var) == pos) (compilerVars state),
                        state))