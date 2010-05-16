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
  addLine $ "#include \"rfx.h\""
  addLine $ "#define __RFX_THREAD_COUNT " ++ (show $ threadsLen)
  addLine $ "enum __rfx_threads {"
  addIndent
  zeroIterator "threadN"
  sequence_ [do
              i <- nextIterator "threadN"
              if not isFirst
                then addString ","
                else return ()
              addLine $ (getThreadName thread) ++ " = "
                          ++ (show i)
             | (thread,isFirst) <- zip threads (True:repeat False)]
  subIndent
  addLine $ "};"
  sequence_ [do
              zeroIterator "stateN"
              let thName = tlString $ threadName thread
              addLine $ "enum __rfx_" ++ thName ++ "_states {"
              addIndent
              sequence_ [do
                          i <- nextIterator "stateN"
                          if not isFirst
                             then addString ","
                             else return ()
                          addLine $ getStateName thread state
                                      ++ " = " ++ (show i)
                         | (state, isFirst) <- zip (threadStates thread) (True:repeat False)]
              subIndent
              addLine $ "};"
             | thread <- threads]
  -- Global vars
  addLine "/* Global vars */"
  globalVars <- getVarsFromScope InGlobal
  sequence_ [varDefenitionCompiler var | var <- globalVars]
  -- Thread vars
  addLine "/* Thread-scope vars */"
  sequence_ [do
              threadVars <- getVarsFromScope $ InThread thread
              sequence_ [ varDefenitionCompiler var
                              | var <- threadVars]
                  | thread <- threads]
  -- States vars
  sequence_ [ do
              addLine $ "/* State-scope vars from thread " ++ threadName thread ++ "*/"
              sequence_ [do
                          addLine $ "/*\tVars form state " ++ stateName state ++  "*/"
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
              sequence_ [do
                          if not isFirst
                             then addString ","
                             else return ()
                          addString "0"
                         | (_, isFirst) <- zip [1..statesCount] (True:repeat False)]
              addLine "};"
              | thread <- threads]
  -- Timers vars
  addLine "/* States timer array */"
  sequence_ [ addLine $ "int " ++ getThreadTimerName thread ++ " = 0;"
              | thread <- threads]
  --
  if (not.null) threads
    then addLine $ "int __rfx_cur_thread = " ++ (getThreadName $ head threads) ++ ";"
    else return ()
  -- User functions
  addLine "/* User functions */"
  sequence_ [funcDefenitionCompiler func | func <- funcs]
  --States funs
  addLine "/* States functions */"
  sequence_ [do
              enterThread thread
              sequence_ [do
                          stateCompiler thread state
                          | state <- threadStates thread]
             | thread <- threads]
  --Rfx timer
  addLine "/*Rfx was here, lol*/"
  addLine "void __rfx_do_timer(int ms)"
  addLine "{"
  addIndent
  sequence_ [do
              let timerName = getThreadTimerName thread
              addLine $ "if (" ++ timerName ++ ">0)" 
              withIndent $ addLine $ timerName ++ " -= ms;"
              addLine $ "else if (" ++ timerName ++ "<0)"
              withIndent $ addLine $ timerName ++ " = 0;"
              | thread <- threads]
  subIndent
  addLine "}"
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
              addLine $ "case " ++ (getThreadName thread ) ++ ":"
              addIndent
              addLine "switch (__rfx_states[__rfx_cur_thread])"
              addLine "{"
              addIndent
              sequence_ [do
                          addLine $ "case " ++ (getStateName thread state) ++ ":"
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
  sequence_ [ if varInitValue var == VoidSemExpr
                 then return ()
                 else statmentCompiler (AssignSt (RValueVar var) (varInitValue var) (varSourcePos var))
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
statmentCompiler (AssignSt rValue expr pos) = do
  let exprType = typeOfExpr expr
  case exprType of
    (ArrayType _ n) -> do
      let (ArraySemExpr exprs) = expr            
      sequence_ [ do
                  let e = exprs !! (fromInteger i)
                  statmentCompiler $ AssignSt
                                   (RValueArrayAccess rValue (NumSemExpr i))
                                   e pos
                  | i <- [0..(n-1)]]
    _ -> do
      makeIndent
      rValueCompiler rValue
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
  let timerName = getThreadTimerName thread
  let arrayName = getThreadStateArrayName thread ++ "["
                  ++ getStateName thread state ++ "]"
  subIndent
  addLine $ "case " ++ show n ++ ":"
  addIndent
  if typeOfExpr expr == BoolType
    then do
      makeIndent
      addString $ "if ("
      exprCompiler expr
      addString ")\n"
      withIndent $
                 addLine $ arrayName ++ " = 0;"
      addLine "else"
      addLine "{"
      withIndent $ do
           addLine $ arrayName ++ " = "
                       ++ show n ++ ";"
           addLine $ "return;"
      addLine "}"
    else do
      addLine $ "if (" ++ arrayName ++ "==0)"
      addLine "{"
      withIndent $ do
           makeIndent
           addString $ timerName ++ " = "
           exprCompiler expr
           addString ";\n"
           addLine $ arrayName ++ " = "
                       ++ show n ++ ";"
           addLine "return;"
      addLine "}"
      addLine $ "else if (" ++ timerName ++ "==0)"
      withIndent $
                 addLine $ arrayName ++ " = 0;"
      addLine "else"
      withIndent $ addLine $ "return;"

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
rValueCompiler :: RValue SemExpr -> Compiler ()
rValueCompiler (RValueVar var) = varCompiler var
rValueCompiler (RValueArrayAccess rValue expr) = do
  rValueCompiler rValue
  addString "["
  exprCompiler expr
  addString "]"


exprCompiler :: SemExpr -> Compiler ()
exprCompiler (NumSemExpr n) = addString $ (show n) ++ " "

exprCompiler (TimeSemExpr t) = addString $ (show t) ++ " "

exprCompiler (StringSemExpr s) = addString $ "\"" ++ s ++ "\" "

exprCompiler (BoolSemExpr b) = addString $ if b then "TRUE " else  "FALSE "

exprCompiler (SubSemExpr e) = do
  addString "( "
  exprCompiler e
  addString ") "

exprCompiler (ArrayAccessSemExpr e i) = do
  addString "("
  exprCompiler e
  addString ")["
  exprCompiler i
  addString "]"  
            
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
                    NumModSemOp    -> "% "
                    BoolAndSemOp   -> "&& "
                    BoolOrSemOp    -> "|| "
                    BoolEqlSemOp   -> "== "
                    BoolNEqlSemOp  -> "!= "
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
               BuildinFunc{biFuncTargetName} -> biFuncTargetName)
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

exprCompiler (ArraySemExpr exprs) = do
  addString "{"
  let addArg [] = addString "}"
      addArg [a] = do
        exprCompiler a
        addString "}"
      addArg (a:as) = do
        exprCompiler a
        addString ", "
        addArg as
  addArg exprs
                           
varCompiler :: Var SemExpr -> Compiler ()
varCompiler v@Var{varConst, varInitValue} = do
  if varConst
     then exprCompiler $ SubSemExpr varInitValue
     else addString $ (getVarFullName v) ++ " "

typeCompiler :: VarType -> Compiler ()
typeCompiler tp = addString $ typeName tp

typeName :: VarType -> String
typeName tp = case tp of
                StringType -> "char*"
                TimeType -> "long int"
                BoolType -> "BOOL"
                VoidType -> "void"
                Int8Type -> "char"
                Int16Type -> "short"
                Int32Type -> "int"
                (ArrayType st _) -> typeName st

arrayTypeSizes :: VarType -> String
arrayTypeSizes (ArrayType st size) = arrayTypeSizes st ++ "[" ++ show size ++ "]"
arrayTypeSizes _ = ""
                                   
varDefenitionCompiler :: Var SemExpr -> Compiler ()
varDefenitionCompiler var@Var{varConst} = do
  if varConst
     then do
       return ()
     --   addString "#define "
     --   addString $ getVarFullName var
     --   if (varInitValue var /= VoidSemExpr)
     --     then do
     --       addString "  ("
     --       exprCompiler (varInitValue var)
     --       addString ")"
     --     else return ()
     else do
       makeIndent
       typeCompiler (varType var)
       addString " "
       addString $ getVarFullName var
       addString $ arrayTypeSizes $ varType var
       if (varInitValue var /= VoidSemExpr)
         then do
           addString " = "
           exprCompiler (varInitValue var)
         else return ()
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

getThreadStateArrayName :: Thread SemExpr -> String
getThreadStateArrayName th = getThreadName th ++ "_state"

getThreadTimerName :: Thread SemExpr -> String
getThreadTimerName th = getThreadName th ++ "_timer"