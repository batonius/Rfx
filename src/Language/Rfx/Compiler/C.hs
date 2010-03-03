{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Compiler.C(programCompiler)
where
import Language.Rfx.Compiler.Common
import Language.Rfx.Structures
import Language.Rfx.Util

programCompiler :: Program SemExpr -> Compiler ()
programCompiler program = do
  let threads = programThreads program
  let threadsLen = length threads
  addLine "/*Rfx was here*/"
  addLine $ "#define __RFX_THREAD_COUNT " ++ (show $ threadsLen)
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
                         | state <- threadStates thread]
              subIndent
              addLine $ "};"
             | thread <- threads]
  -- Global vars
  globalVars <- getVarsFromScope InGlobal
  sequence_ [varDefenitionCompiler var | var <- globalVars]
  -- Thread vars
  sequence_ [do
              threadVars <- getVarsFromScope $ InThread thread
              sequence_ [ varDefenitionCompiler var
                              | var <- threadVars]
                  | thread <- threads]
  -- States array
  addLine $ "int __rfx_states[__RFX_THREAD_COUNT] = {" ++ (concat $ map
                                                           (\th -> (getStateName th (head $ threadStates th)) ++ ", ")
                                                           threads)
              ++ "};"
  addLine $ "int __rfx_cur_thread = " ++ (getThreadName $ head threads) ++ ";"
  --States funs
  sequence_ [do
              enterThread thread
              sequence_ [do
                          stateCompiler thread state
                          | state <- threadStates thread]
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
                         | state <- threadStates thread]
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

stateCompiler :: Thread SemExpr -> ThreadState SemExpr -> Compiler ()
stateCompiler thread state = do
  addLine "\nvoid"
  addLine $ (getStateName thread state) ++ "_fun()"
  addLine "{"
  addIndent
  stateVars <- getVarsFromScope $ InState thread state
  sequence_ [varDefenitionCompiler var | var <- stateVars]
  sequence_ [do
              statmentCompiler st
             | st <- stateStatments state]
  subIndent
  addLine "}"

statmentsCompiler :: [Statment SemExpr] -> Compiler ()
statmentsCompiler sts = do
  addLine "{"
  addIndent
  sequence_ [statmentCompiler statment
             | statment <- sts]
  subIndent
  addLine "}"

statmentCompiler :: Statment SemExpr -> Compiler ()
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

-- statmentCompiler (NextSt ThreadState{stateName}) = do
--   state <- getState (stateName nextState)
--   curThread <- getCurrentThread
--   makeIndent
--   addString "__rfx_states["
--   addString $ getThreadName curThread
--   addString "] = "
--   addString $ getStateName curThread state
--   addString ";\n"

statmentCompiler BreakSt = addLine "break;"

statmentCompiler (FunSt fun) = do
  makeIndent
  exprCompiler fun
  addString ";\n"

statmentCompiler _ = error "Not implemented yet"

exprCompiler :: SemExpr -> Compiler ()
exprCompiler (NumSemExpr n) = addString $ (show n) ++ " "

exprCompiler (StringSemExpr s) = addString $ "\"" ++ s ++ "\""
                           
exprCompiler (SubSemExpr e) = do
  addString "( "
  exprCompiler e
  addString ") "

exprCompiler (VarSemExpr v) = varCompiler v
exprCompiler (OpSemExpr op le re) = do
  exprCompiler le
  addString $ case op of
                NumPlusSemOp     -> "+ "
                NumMinusSemOp    -> "- "
                NumMulSemOp      -> "* "
                NumEqlSemOp      -> "== "
                NumGrSemOp       -> "> "
                NumLsSemOp       -> "< "
                NumDivSemOp      -> "/ "
                _          -> ""
  exprCompiler re

exprCompiler (FunSemExpr ()) = do
  addString "Fun goes here"
  -- addString funName
  -- addString "("
  -- let addArg [] = addString ")"
  --     addArg [a] = do
  --       exprCompiler a
  --       addString ")"
  --     addArg (a:as) = do
  --       exprCompiler a
  --       addString ", "
  --       addArg as
  -- addArg args

varCompiler :: Var SemExpr -> Compiler ()
varCompiler v = do
  addString $ (getVarFullName v) ++ " "

typeCompiler :: VarType -> Compiler ()
typeCompiler tp = addString $ case tp of
                                StringType -> "char*"
                                _ -> "int"

varDefenitionCompiler :: Var SemExpr -> Compiler ()
varDefenitionCompiler var = do
  makeIndent
  typeCompiler (varType var)
  addString " "
  addString $ getVarFullName var
  addString " = "
  exprCompiler (varInitValue var)
  addString ";\n"

getVarFullName :: Var SemExpr -> String
getVarFullName var = "__rfx__" ++ (case (varScope var) of
                                    InGlobal -> ""
                                    InThread Thread{threadName} -> (tlString threadName) ++ "_"
                                    InState Thread{threadName}
                                            ThreadState{stateName} ->
                                                (tlString threadName) ++ "_"
                                                 ++ (tlString stateName) ++ "_")
                     ++ (tlString (varName var))

getThreadName :: Thread SemExpr -> String
getThreadName th = "__rfx_thread__" ++ (tlString $ threadName th)

getStateName :: Thread SemExpr -> ThreadState SemExpr -> String
getStateName th st = "__rfx_state__" ++ (tlString $ threadName th) ++ "_" ++ (tlString $ stateName st)
