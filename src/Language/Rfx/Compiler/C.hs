{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Compiler.C(programCompiler)
where
import Language.Rfx.Compiler.Common
import Language.Rfx.Structures
import Language.Rfx.Util

programCompiler :: Program SemExpr -> Compiler ()
programCompiler program = do
  let threads = programThreads program
  let funcs = filter (not.buildinFunc) $ programFuncs program
  let threadsLen = length threads
  addLine "/*Rfx was here, lol*/"
  addLine "#define XOR(x,y) ((x) ? !(y) : (y))"
  addLine "#define BOOL int"
  addLine "#define TRUE 1"
  addLine "#define FALSE 0"
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
  addLine "/* Global vars */"
  globalVars <- getVarsFromScope InGlobal
  sequence_ [varDefenitionCompiler var | var <- globalVars]
  -- User functions
  addLine "/* User functions */"
  sequence_ [funcDefenitionCompiler func | func <- funcs]
  -- Thread vars
  addLine "/* Thread-scope vars */"
  sequence_ [do
              threadVars <- getVarsFromScope $ InThread thread
              sequence_ [ varDefenitionCompiler var
                              | var <- threadVars]
                  | thread <- threads]
  -- States vars
  sequence_ [ do
              addLine $ "/* State-scope vars from thread" ++ threadName thread ++ "*/"
              sequence_ [do
                          addLine $ "/*\tVars form state" ++ stateName state ++  "*/"
                          stateVars <- getVarsFromScope $ InState thread state
                          sequence_ [ varDefenitionCompiler var
                                      | var <- stateVars]
                          | state <- threadStates thread]
              | thread <- threads]
  -- States array
  addLine "/* Thread state array */"
  addLine $ "int __rfx_states[__RFX_THREAD_COUNT] = {" ++ (concat $ map
    (\th -> if (not.null) (threadStates th)
            then (getStateName th (head $ threadStates th)) ++ ", "
            else "ERROR LOL")
    threads) ++ "};"
  -- States state array
  addLine "/* States state array */"
  sequence_ [do
              let statesCount = length $ threadStates thread;
              addLine $ "int " ++ (getThreadStateArrayName thread)
                     ++ "[" ++ (show statesCount) ++ "] = {"
              sequence_ [ addString "0, " | _ <- [1..statesCount]]
              addLine "};"
              | thread <- threads]
  --
  if (not.null) threads
    then addLine $ "int __rfx_cur_thread = " ++ (getThreadName $ head threads) ++ ";"
    else return ()
  --States funs
  addLine "/* States functions */"
  sequence_ [do
              enterThread thread
              sequence_ [do
                          stateCompiler thread state
                          | state <- threadStates thread]
             | thread <- threads]
  --Main rfx fun
  addLine "/* Scheduler */"
  addLine "void __rfx_next()"
  addLine "{"
  addIndent
  if (not.null) threads
     then do
       addLine $ "if (__rfx_cur_thread>" ++ (getThreadName $ last threads) ++ ")"
       addIndent
       addLine $ "__rfx_cur_thread = " ++ (getThreadName $ head threads) ++ ";"
       subIndent
     else return ()
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

funcDefenitionCompiler :: Func SemExpr -> Compiler ()
funcDefenitionCompiler func@UserFunc{uFuncArgs, uFuncStatments, uFuncRetType} = do
  let makeArgList :: [Var SemExpr] -> String
      makeArgList = makeArgList' []
          where makeArgList' acc [] = acc
                makeArgList' acc (var@Var{varType}:vars) =
                    makeArgList'
                    ((if null acc then "" else acc++", ") ++ (typeName varType) ++ " " ++ (getVarFullName var))
                    vars
  typeCompiler uFuncRetType
  addString " "
  addString $ getFuncName func
  addString "("
  addString $ makeArgList uFuncArgs
  addString ")\n"
  addLine "{"
  addIndent
  funcVars <-  getVarsFromScope $ InFunction func
  let funcVars' = filter ((/=VoidSemExpr).varInitValue) funcVars
  sequence_ [varDefenitionCompiler var | var <- funcVars']
  sequence_ [do
              statmentCompiler st
             | st <- uFuncStatments]
  subIndent
  addLine "}"
funcDefenitionCompiler _ = error "No defenitino for buildin functions"

stateCompiler :: Thread SemExpr -> ThreadState SemExpr -> Compiler ()
stateCompiler thread state = do
  enterState state
  addLine "\nvoid"
  addLine $ (getStateName thread state) ++ "_fun()"
  addLine "{"
  addIndent
  addLine $ "switch (" ++ getThreadStateArrayName thread
          ++ "[" ++ (getStateName thread state) ++ "])"
  addLine "{"
  addLine "case 0:"
  addIndent
  stateVars <- getVarsFromScope $ InState thread state
  sequence_ [ statmentCompiler (AssignSt var (varInitValue var) (varSourcePos var))
                  | var <- stateVars]
  sequence_ [do
              statmentCompiler st
             | st <- stateStatments state]
  addLine "return;"
  subIndent
  addLine "}"
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
statmentCompiler (AssignSt var expr _) = do
  makeIndent
  varCompiler var
  addString "= "
  exprCompiler expr
  addString $ ";\n"

statmentCompiler (IfSt expr sts _) = do
  makeIndent
  addString "if ("
  exprCompiler expr
  addString ")\n"
  statmentsCompiler sts

statmentCompiler (IfElseSt expr sts1 sts2 pos) = do
  statmentCompiler (IfSt expr sts1 pos)
  addLine "else"
  statmentsCompiler sts2

statmentCompiler (WhileSt expr sts _) = do
  makeIndent
  addString "while ("
  exprCompiler expr
  addString ")\n"
  statmentsCompiler sts

statmentCompiler (WaitSt expr n _) = do
  thread <- getCurrentThread
  state <- getCurrentState
  subIndent
  addLine $ "case " ++ show n ++ ":"
  addIndent
  makeIndent
  addString $ "if ("
  exprCompiler expr
  addString ")\n"
  withIndent $
             addLine $ getThreadStateArrayName thread ++ "["
                         ++ getStateName thread state ++ "] = 0;"
  addLine "else"
  addLine "{"
  withIndent $ do
    addLine $ getThreadStateArrayName thread ++ "["
                ++ getStateName thread state ++ "] = "
                ++ show n ++ ";"
    addLine $ "return;"
  addLine "}"

statmentCompiler (NextSt ThreadState{stateName} _) = do
  state <- getState stateName
  curThread <- getCurrentThread
  makeIndent
  addString "__rfx_states["
  addString $ getThreadName curThread
  addString "] = "
  addString $ getStateName curThread state
  addString ";\n"

statmentCompiler (ReturnSt retExpr _) = do
  makeIndent
  addString "return "
  if voidExpr retExpr
    then return ()
    else exprCompiler retExpr
  addString ";\n"

statmentCompiler BreakSt{} = addLine "break;"

statmentCompiler (FunSt fun _) = do
  makeIndent
  exprCompiler fun
  addString ";\n"

--statmentCompiler _ = error "Not implemented yet"

exprCompiler :: SemExpr -> Compiler ()
exprCompiler (NumSemExpr n) = addString $ (show n) ++ " "

exprCompiler (TimeSemExpr t) = addString $ (show t) ++ " "

exprCompiler (StringSemExpr s) = addString $ "\"" ++ s ++ "\" "

exprCompiler (BoolSemExpr b) = addString $ if b then "TRUE " else  "FALSE "

exprCompiler (SubSemExpr e) = do
  addString "( "
  exprCompiler e
  addString ") "

exprCompiler (VarSemExpr v) = varCompiler v
exprCompiler (OpSemExpr op le re _) = do
  if op == BoolXorSemOp
    then do
      addString("XOR(")
      exprCompiler le
      addString(", ")
      exprCompiler re
      addString(")")
    else do
      exprCompiler le
      addString $ case op of
                    NumPlusSemOp   -> "+ "
                    NumMinusSemOp  -> "- "
                    NumMulSemOp    -> "* "
                    NumEqlSemOp    -> "== "
                    NumNEqlSemOp   -> "!="
                    NumGrSemOp     -> "> "
                    NumGrEqlSemOp  -> ">= "
                    NumLsSemOp     -> "< "
                    NumLsEqlSemOp  -> "<= "
                    NumDivSemOp    -> "/ "
                    BoolAndSemOp   -> "&& "
                    BoolOrSemOp    -> "|| "
                    TimePlusSemOp  -> "+ "
                    TimeMinusSemOp -> "- "
                    TimeEqlSemOp   -> "== "
                    TimeNEqlSemOp  -> "!= "
                    TimeGrSemOp    -> "> "
                    TimeLsSemOp    -> "< "
                    TimeGrEqlSemOp -> ">= "
                    TimeLsEqlSemOp -> "<= "
                    _              -> ""
      exprCompiler re

exprCompiler (FunSemExpr func args) = do
  addString (case func of
               uf@UserFunc{} -> getFuncName uf
               BuildinFunc{biFuncName} -> biFuncName)
  addString "("
  let addArg [] = addString ")"
      addArg [a] = do
        exprCompiler a
        addString ")"
      addArg (a:as) = do
        exprCompiler a
        addString ", "
        addArg as
  addArg args

exprCompiler VoidSemExpr = error "Compiling void expression"

varCompiler :: Var SemExpr -> Compiler ()
varCompiler v = do
  addString $ (getVarFullName v) ++ " "

typeCompiler :: VarType -> Compiler ()
typeCompiler tp = addString $ typeName tp

typeName tp = case tp of
                StringType -> "char*"
                TimeType -> "long int"
                BoolType -> "BOOL"
                VoidType -> "void"
                Int8Type -> "char"
                Int16Type -> "short"
                Int32Type -> "int"
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
                                    InThread Thread{threadName} -> "th_" ++ (tlString threadName) ++ "_"
                                    InState Thread{threadName} ThreadState{stateName} ->
                                        "th_" ++ (tlString threadName) ++ "_" ++ (tlString stateName) ++ "_"
                                    InFunction UserFunc{uFuncName} -> "func_" ++ uFuncName ++ "_"
                                    _ -> error "Variable in build-in function, wtf?")
                     ++ (tlString (varName var))

getThreadName :: Thread SemExpr -> String
getThreadName th = "__rfx_thread__" ++ (tlString $ threadName th)

getStateName :: Thread SemExpr -> ThreadState SemExpr -> String
getStateName th st = "__rfx_state__" ++ (tlString $ threadName th) ++ "_" ++ (tlString $ stateName st)

getFuncName :: Func SemExpr -> String
getFuncName UserFunc{uFuncName} = "__rfx_func__" ++ (tlString $ uFuncName)
getFuncName _ = error "Get buildin function name, wtf?"

getThreadStateArrayName th = getThreadName th ++ "_state"