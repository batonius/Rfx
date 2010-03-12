{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Validator(validateProgram)
where
import Text.Parsec.Pos(newPos, SourcePos)
import Language.Rfx.Error
import Language.Rfx.Structures
import qualified Data.Map as Map
import Control.Exception hiding (try)

zeroPos = newPos "" 0 0
    
validateProgram :: Program SynExpr -> Program SemExpr
validateProgram Program{programThreads=prThs, programVars=prVrs, programFuncs=prFuncs} = program'
    where program = Program{programVars=validateVars program prVrs
                           ,programThreads=map (validateThread program) prThs
                           ,programFuncs=buildinFuncs++(map (validateFunc program) prFuncs)}
          program' = Program{programThreads=(programThreads program)
                            ,programVars=replaceVars program (programVars program)
                            ,programFuncs=(programFuncs program)}
          replaceVars :: Program SemExpr -> [Var SemExpr] -> [Var SemExpr]
          replaceVars _ [] = []
          replaceVars pr (var@(Var{varScope}):vars) = var{varScope=trScope pr varScope} : (replaceVars pr vars)
          trScope _ InGlobal = InGlobal
          trScope pr (InFunction f) = InFunction (funcByName pr (FuncName (funcName f) zeroPos))
          trScope pr (InThread Thread{threadName}) = InThread $ threadByName pr threadName zeroPos
          trScope pr (InState Thread{threadName} ThreadState{stateName}) =
              InState (threadByName pr threadName zeroPos) (stateByName pr threadName stateName zeroPos)

validateThread :: Program SemExpr -> Thread SynExpr -> Thread SemExpr
validateThread pr Thread{threadName, threadStates} = thread
    where thread = Thread{threadName, threadStates =
                              map (validateState pr thread) threadStates}

validateState :: Program SemExpr -> Thread SemExpr -> ThreadState SynExpr -> ThreadState SemExpr
validateState pr thread ThreadState{stateName, stateStatments} = state
    where state = ThreadState{stateName, stateStatments =
                                map (validateStatment pr (InState thread state)) stateStatments}

validateVars :: Program SemExpr -> [Var SynExpr] -> [Var SemExpr]
validateVars pr vars = validateVars' vars [] where
    validateVars' :: [Var SynExpr] -> [Var SemExpr] -> [Var SemExpr]
    validateVars' [] vVars = vVars
    validateVars' (var@Var{varScope, varInitValue,
                           varType=varType, varName, varSourcePos}:uVars) vVars =
      let pr' = Program{programThreads=[], programVars=vVars, programFuncs=[]}
          vInitVal = validateExpr pr' InGlobal varInitValue
          vScope = transScope pr varScope
          vVarType = validateType varType varSourcePos
      in if (not.null) $ usedVars vInitVal
           then throw $ VarInVarInitSemExc var
           else if (typeOfExpr pr' InGlobal vInitVal /= vVarType)
                  then throw $ VarInitWrongTypeSemExc var
                  else let sameVars = filter (\Var{varName=vn} -> vn == varName) $ scopeVars vScope vVars
                       in if (not.null)  sameVars
                            then throw $ VarAlreadyExistsSemExc var (Language.Rfx.Structures.varSourcePos (head sameVars))
                            else validateVars' uVars (Var{varName
                                                         ,varType=vVarType
                                                         ,varSourcePos
                                                         ,varScope = vScope
                                                         ,varInitValue = vInitVal}
                                                      :vVars)

usedVars :: SemExpr -> [Var SemExpr]
usedVars (VarSemExpr var) = [var]
usedVars (OpSemExpr _ leftExpr rightExpr ) = (usedVars leftExpr) ++ (usedVars rightExpr)
usedVars (SubSemExpr subExpr) = usedVars subExpr
usedVars (FunSemExpr _ args) = concat $ map usedVars args
usedVars _ = []

typeOfExpr :: Program SemExpr -> ProgramPos SemExpr -> SemExpr -> VarType
typeOfExpr _ _ (NumSemExpr _) = Int8Type -- TODO
typeOfExpr _ _ (BoolSemExpr _) = BoolType
typeOfExpr _ _ (TimeSemExpr _) = TimeType
typeOfExpr _ _ (StringSemExpr _) = StringType
typeOfExpr _ _ (VarSemExpr Var{varType}) = varType
typeOfExpr _ _ (FunSemExpr func _ ) = funcRetType func
typeOfExpr pr scope (SubSemExpr se) = typeOfExpr pr scope se
typeOfExpr _ _ (OpSemExpr semOp _ _ ) = opType
    where (_ , (_, _, opType)) = let ops = filter ((==semOp).fst) semOpTypes
                                 in if null ops
                                    then error $ "No such oper" ++ (show semOp) -- User error? No
                                    else head ops

scopeVars :: (Expression a) => ProgramPos a -> [Var a] -> [Var a]
scopeVars scope = filter (\Var{varScope} -> scope `posChildOf` varScope)

transScope :: Program SemExpr -> ProgramPos SynExpr -> ProgramPos SemExpr
transScope _ InGlobal = InGlobal
transScope _ (InFunction (FuncName f _)) = InFunction $ (UserFunc{uFuncName=f}::(Func SemExpr))
transScope _ (InThread Thread{threadName}) = InThread $ Thread {threadName, threadStates=[]}
transScope _ (InState Thread{threadName} ThreadState{stateName}) =
    InState (Thread{threadName, threadStates=[]}) (ThreadState {stateName, stateStatments=[]})

threadByName :: Program SemExpr -> String -> SourcePos -> Thread SemExpr
threadByName ~Program{programThreads} thName pos = let threads = filter (\Thread{threadName} -> threadName == thName) programThreads
                                                   in if null threads
                                                      then throw $ NoSuchThreadSemExc thName pos
                                                      else head threads

stateByName :: Program SemExpr -> String -> String -> SourcePos -> ThreadState SemExpr
stateByName pr thName stName pos = let states = filter (\ThreadState{stateName} -> stateName == stName) $ threadStates $ threadByName pr thName pos
                                   in if null states
                                      then throw $ NoSuchStateSemExc thName stName pos
                                      else head states

funcByName :: Program SemExpr -> FuncName -> Func SemExpr
funcByName ~Program{programFuncs} fun@(FuncName fn _) = let funcs = filter ((==fn).funcName) programFuncs
                                                          in if null funcs
                                                             then throw $ NoSuchFuncSemExc fun
                                                             else head $ funcs
                                           
varByName :: Program SemExpr -> ProgramPos SemExpr -> VarName -> Var SemExpr
varByName ~Program{programVars} scope varName  =
    case varName of
      VarName vName _-> let vars = filter (\Var{varName=n} -> vName == n)
                                   $ scopeVars scope programVars
                        in if null vars
                             then throw $ NoSuchVarSemExc varName
                             else head vars
      LongVarName thName vName _ -> let vars = filter (\Var{varName, varScope} ->
                                                            case varScope of
                                                              InThread Thread{threadName} -> (thName==threadName)
                                                                       && (varName==vName)
                                                              _ -> False) programVars
                                             -- filter (\Var{varName=n} -> vName == n)
                                             --     $ scopeVars (InThread $ threadByName pr thName pos) programVars
                                      in if null vars
                                           then throw $ NoSuchVarSemExc varName
                                           else head vars


validateFunc :: Program SemExpr -> Func SynExpr -> Func SemExpr
validateFunc pr UserFunc{uFuncName, uFuncArgs, uFuncStatments, uFuncRetType, uFuncPos} = func
    where
      func = UserFunc{uFuncName, uFuncArgs=vArgs, uFuncStatments=vStatments, uFuncRetType=vFuncType, uFuncPos}
      vFuncType = validateType uFuncRetType uFuncPos
      self = funcByName pr $ FuncName uFuncName uFuncPos 
      vArgs = map (\Var{varName} -> varByName pr (InFunction self) (VarName varName uFuncPos)) uFuncArgs 
      vStatments = map (validateStatment pr (InFunction func)) uFuncStatments

validateFunc _ _ = error "Wut?"       
                                                
validateExpr :: Program SemExpr -> ProgramPos SemExpr -> SynExpr -> SemExpr
validateExpr pr scope expr = validateExpr' pr scope $ dropSubExpr $ applyOpPriority expr
    where
      validateExpr' :: Program SemExpr -> ProgramPos SemExpr -> SynExpr -> SemExpr
      validateExpr' _ _ (NumSynExpr n) = NumSemExpr n
      validateExpr' _ _ (BoolSynExpr b) = BoolSemExpr b
      validateExpr' _ _ (StringSynExpr s) = StringSemExpr s
      validateExpr' _ _ (TimeSynExpr t) = TimeSemExpr t
      validateExpr' pr scope (VarSynExpr varName) = VarSemExpr $ varByName pr scope varName
      validateExpr' pr scope (FunSynExpr f@(FuncName _ pos) args ) = let vArgs = map (validateExpr' pr scope) args
                                                                         fun :: Func SemExpr
                                                                         fun = funcByName pr f
                                                                     in if (funcArgsTypes fun) == (map (typeOfExpr pr scope) vArgs)
                                                                        then FunSemExpr fun vArgs
                                                                        else throw $ FuncCallWrongType fun pos
      validateExpr' pr scope (SubSynExpr e) = SubSemExpr $ validateExpr' pr scope e
      validateExpr' pr scope exp@(OpSynExpr op lExpr rExpr _) =
          let vlExpr = validateExpr' pr scope lExpr
              vrExpr = validateExpr' pr scope rExpr
              vlType = typeOfExpr pr scope vlExpr
              vrType = typeOfExpr pr scope vrExpr
              (Just synOps) = Map.lookup op opTypes
              (semOp,_) = let ops = filter (\(so, (lt, rt, _)) ->
                                            (lt == vlType) && (rt == vrType) && (so `elem` synOps))
                                    $ semOpTypes
                          in if null ops
                             then throw $ OpSemExc exp
                             else head ops
          in OpSemExpr semOp vlExpr vrExpr

validateStatment :: Program SemExpr -> ProgramPos SemExpr -> Statment SynExpr -> Statment SemExpr
validateStatment pr scope st@(IfSt ex sts) = let valEx = validateExpr pr scope ex
                                             in if (typeOfExpr pr scope valEx == BoolType)
                                                then IfSt valEx $ map (validateStatment pr scope) sts
                                                else throw $ IfNotBoolSemExc st

validateStatment pr scope st@(IfElseSt ex ifSts elseSts) = let valEx = validateExpr pr scope ex
                                                           in if (typeOfExpr pr scope valEx == BoolType)
                                                              then IfElseSt valEx (map (validateStatment pr scope) ifSts)
                                                                       (map (validateStatment pr scope) elseSts)
                                                              else throw $ IfNotBoolSemExc st

validateStatment _ _ BreakSt = BreakSt

validateStatment pr ~(InState Thread{threadName} _) (NextSt (ThreadState{stateName}) pos) =
    NextSt (stateByName pr threadName stateName pos) pos

validateStatment pr scope st@(WhileSt ex sts) = let valEx = validateExpr pr scope ex
                                                in if (typeOfExpr pr scope valEx == BoolType)
                                                   then WhileSt valEx $ map (validateStatment pr scope) sts
                                                   else throw $ WhileNotBoolSemExc st

validateStatment pr scope st@(AssignSt varName expr) = let valExpr = validateExpr pr scope expr
                                                           valType = typeOfExpr pr scope valExpr
                                                           var = varByName pr scope varName
                                                       in if (varType var == valType)
                                                          then AssignSt var valExpr
                                                          else throw $ AssignWrongType st

validateStatment pr scope (FunSt e) = FunSt $ validateExpr pr scope e
                                                               
priorityList :: [[SynOper]]
priorityList = [[AndSynOp
                ,OrSynOp
                ,XorSynOp]
               ,[EqlSynOp
                ,NEqlSynOp
                ,GrSynOp
                ,LsSynOp
                ,GrEqSynOp
                ,LsEqSynOp]
               ,[PlusSynOp
                ,MinusSynOp]
               ,[MulSynOp
                ,DivSynOp]
               ]

applyOpPriority :: SynExpr -> SynExpr
applyOpPriority (SubSynExpr e) = SubSynExpr $ applyOpPriority e
applyOpPriority (FunSynExpr fun args) = FunSynExpr fun $ map (dropSubExpr.applyOpPriority) args
applyOpPriority oe@(OpSynExpr op le re pos) = if opExpr re
                                              then applyOpPriority' priorityList oe oe
                                              else OpSynExpr op (applyOpPriority le) (applyOpPriority re) pos
    where applyOpPriority' :: [[SynOper]] -> SynExpr -> SynExpr -> SynExpr
          applyOpPriority' [] exprHead (OpSynExpr eo le re pos) = (OpSynExpr eo le (applyOpPriority' [] exprHead re) pos)
          applyOpPriority' [] _ e = applyOpPriority e
          applyOpPriority' pl@(op:ops) exprHead oe@(OpSynExpr eo le re pos) = if eo `elem` op
                                                                              then let newLe = if exprHead == oe
                                                                                               then applyOpPriority le
                                                                                               else changeExprVar exprHead oe $ applyOpPriority le
                                                                                   in OpSynExpr
                                                                                      eo
                                                                                      (SubSynExpr $ applyOpPriority' ops newLe newLe)
                                                                                      (applyOpPriority' pl re re)
                                                                                      pos
                                                                              else applyOpPriority' pl exprHead re
          applyOpPriority' (_:ops) exprHead _ = SubSynExpr $ applyOpPriority' ops exprHead exprHead
          changeExprVar :: SynExpr -> SynExpr -> SynExpr -> SynExpr
          changeExprVar (SubSynExpr e) from to = SubSynExpr $ changeExprVar e from to
          changeExprVar (OpSynExpr op le re pos) from to = OpSynExpr op le (if re == from then to
                                                                            else (changeExprVar re from to)) pos
          changeExprVar e _ _ = e
applyOpPriority e = e

dropSubExpr e = let dse = dropSubExpr' e
                in case dse of
                     (SubSynExpr se) -> se
                     _ -> dse
    where
      dropSubExpr' (OpSynExpr op le re pos) = OpSynExpr op (dropSubExpr' le) (dropSubExpr' re) pos
      dropSubExpr' (SubSynExpr (SubSynExpr se)) = dropSubExpr' $ SubSynExpr se
      dropSubExpr' (SubSynExpr se) = if constExpr se || varExpr se then se
                                     else SubSynExpr $ dropSubExpr' se
      dropSubExpr' (FunSynExpr fun args) = FunSynExpr fun $ map dropSubExpr' args
      dropSubExpr' se = se

validateType :: VarTypeName -> SourcePos -> VarType
validateType (VarTypeName typeName) pos = case Map.lookup typeName
                                             $ Map.fromList
                                             $ [("int8", Int8Type)
                                               ,("bool", BoolType)
                                               ,("string", StringType)
                                               ,("time", TimeType)
                                               ,("ЦЕЛ8", Int8Type)
                                               ,("ЛОГ", BoolType)
                                               ,("СТРОКА", StringType)
                                               ,("ВРЕМЯ", TimeType)]
                                          of Nothing -> throw $ NoSuchTypeSemExc typeName pos
                                             Just vVarType -> vVarType
                   