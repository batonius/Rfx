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
programCompiler program = do
  let threads = programThreads program
  let threadsLen = length threads
  addLine "//Rfx was here"
  addLine $ "#define __RFX_THREAD_COUNT " ++ (show $ length threads)
  addLine $ "enum __rfx_threads {"
  addIndent
  zeroIterator "threadN"
  sequence_ [do
              i <- nextIterator "threadN"
              addLine $ (getThreadName thread) ++ " = "
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
                          addLine $ getStateName thread state 
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
  -- States array
  addLine $ "int __rfx_states[" ++ (show threadsLen)
              ++ "] = {" ++ (concat $ map
                                        (\th -> (getStateName th (head $ states th)) ++ ", ")
                                        threads)
              ++ "};"
  addLine $ "int __rfx_cur_thread = " ++ (getThreadName $ head threads) ++ ";"
  --States funs 
  sequence_ [do
              let thName = tlString $ threadName thread
              sequence_ [do
                          stateCompiler thName state
                          | state <- states thread]
             | thread <- threads]
  --Main rfx fun
  addLine "void __rfx_next()"
  addLine "{"
  addIndent
  addLine $ "if (__rfx_cur_thread>" ++ (getThreadName $ last threads) ++ ")"
  addIndent
  addLine $ "__rfx_cur_thread = " ++ (getThreadName $ head threads) ++ ";"
  subIndent
  addLine "switch (__rfx_cur_thread)"
  addLine "{"
  addIndent
  sequence_ [do
              addLine $ (getThreadName thread ) ++ ":"
              addIndent
              addLine "switch (__rfx_states[__rfx_cur_thread])"
              addLine "{"
              addIndent
              sequence_ [do
                          addLine $ (getStateName thread state) ++ ":"
                          addIndent
                          addLine $ (getStateName thread state) ++ "_fun();"
                          addLine "break;"
                          subIndent
                         | state <- states thread]
              subIndent
              addLine "}"
              addLine "break;"
              subIndent              
              | thread <- threads]
  subIndent
  addLine "}"
  addLine "__rfx_cur_thread++;"
  subIndent
  addLine "}"

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

statmentsCompiler :: [Statment] -> Compiler ()
statmentsCompiler sts = do
  addLine "{"
  addIndent
  sequence_ [statmentCompiler statment
             | statment <- sts]
  subIndent
  addLine "}"
          
statmentCompiler :: Statment -> Compiler ()
statmentCompiler (AssignSt var expr) = do
  makeIndent
  varCompiler var
  addString "= "
  exprCompiler expr
  addString $ ";\n"

statmentCompiler (IfSt expr sts) = do
  makeIndent
  addString "if ("
  exprCompiler expr
  addString ")\n"
  statmentsCompiler sts

statmentCompiler (IfElseSt expr sts1 sts2) = do
  statmentCompiler (IfSt expr sts1)
  addLine "else"
  statmentsCompiler sts2
                    
statmentCompiler (WhileSt expr sts) = do
  makeIndent
  addString "while ("
  exprCompiler expr
  addString ")\n"
  statmentsCompiler sts

statmentCompiler BreakSt = addLine "break;"
            
statmentCompiler _ = error "Not implemented yet"
                     
exprCompiler :: Expr -> Compiler ()
exprCompiler expr = case expr of
                      (NumExpr n) -> addString $ (show n) ++ " "
                      (SubExpr e) -> do
                                addString "( "
                                exprCompiler e
                                addString ") "
                      (VarExpr v) -> varCompiler v
                      (OpExpr op le re) -> do
                                exprCompiler le
                                addString $ case op of
                                              PlusOp     -> "+ "
                                              MinusOp    -> "- "
                                              MulOp      -> "* "
                                              DivOp      -> "/ "
                                              EqualityOp -> "== "
                                              GrOp       -> "> "
                                              LsOp       -> "< "
                                              GrEqOp     -> ">= "
                                              LsEqOp     -> "<= "
                                              _          -> ""
                                exprCompiler re

varCompiler :: Var -> Compiler ()
varCompiler v = do
  varToPrint <- case varType v of
                 CheckMeType -> do
                                threadVars <- getVars $ varScope v
                                case filter (\var -> (varName var) == (varName v)) threadVars of
                                  [onlyVar] -> return onlyVar
                                  _ -> return $ error $ "No such var " ++ (varName v) ++ " " ++ (show $ varScope v)
                 _ -> return v
  addString $ (getVarFullName varToPrint) ++ " "
                                             
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

getThreadName :: Thread -> String
getThreadName th = "__rfx_thread_" ++ (tlString $ threadName th)

getStateName :: Thread -> ThreadState -> String
getStateName th st = "__rfx_state_" ++ (tlString $ threadName th) ++ "_" ++ (tlString $ stateName st)
                        
getVars :: ProgramPos -> Compiler [Var]
getVars pos = State(\state -> 
                    (Set.toList $ Set.filter (\ var -> (varScope var) == pos) (compilerVars state),
                        state))