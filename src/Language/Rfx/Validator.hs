{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Validator(validateProgram)
where
import Text.ParserCombinators.Parsec
import Language.Rfx.Error
import Language.Rfx.Structures
import Data.Char(toUpper)
import qualified Data.Map as Map
import Control.Exception hiding (try)

-- Validate functions
validateProgram :: Program SynExpr -> Program SemExpr
validateProgram Program{programThreads=prThs, programVars=prVrs, programFuncs=prFuncs} = program'
    where program = Program{programVars=validateVars program prVrs
                           ,programThreads=map (validateThread program) prThs
                           ,programFuncs=buildinFuncs++(map (validateFunc program) prFuncs)}
          program' = case duplicatesInList (programThreads program) of
                       Nothing -> program
                       Just Thread{threadName} -> throw $ ThreadAlreadyExistsSemExc threadName

validateThread :: Program SemExpr -> Thread SynExpr -> Thread SemExpr
validateThread pr Thread{threadName, threadStates=threadStates'} = thread'
    where thread = Thread{threadName, threadStates =
                              map (validateState pr thread) threadStates'}
          thread'= case duplicatesInList (threadStates thread) of
                     Nothing -> thread
                     Just ThreadState{stateName} -> throw $ StateAlreadyExistsSemExc threadName stateName

validateState :: Program SemExpr -> Thread SemExpr -> ThreadState SynExpr -> ThreadState SemExpr
validateState pr thread ThreadState{stateName, stateStatments} = state
    where state = ThreadState{stateName, stateStatments =
                                map (validateStatment pr (InState thread state)) stateStatments}

validateVars :: Program SemExpr -> [Var SynExpr] -> [Var SemExpr]
validateVars pr vars = validateVars' vars [] where
    validateVars' :: [Var SynExpr] -> [Var SemExpr] -> [Var SemExpr]
    validateVars' [] vVars = vVars
    validateVars' (var@Var{varScope, varInitValue, varArg
                           ,varType, varName, varSourcePos, varConst=varConst'}:uVars) vVars =
      let pr' = pr{programVars=vVars}
          vInitVal = validateExpr pr' InGlobal varInitValue
          vScope = transScope pr varScope varSourcePos
          vVarType = validateType varType varSourcePos
          uv = usedVars vInitVal
      in if (varConst' && ((not.null) uv)) || ((not varConst') && (not (all varConst uv)))
           then throw $ VarInVarInitSemExc var
           else if (vVarType /= VoidType) && (typeOfExpr vInitVal /= VoidType)
                    && (not (typeOfExpr vInitVal `typeCanBe` vVarType))
          --(not varArg) && (not (typeOfExpr vInitVal `typeCanBe` vVarType) || (vVarType == VoidType))  -- TODO Make exception
                then throw $ VarInitWrongTypeSemExc var
                else let sameVars = filter (\Var{varName=vn} -> vn == varName) $ scopeVars vScope vVars
                     in if (not.null)  sameVars
                        then throw $ VarAlreadyExistsSemExc var (Language.Rfx.Structures.varSourcePos (head sameVars))
                        else if varConst' && (vInitVal == VoidSemExpr)
                               then throw $ ConstVarWithoutInitSemExc var
                               else validateVars' uVars (Var{varName
                                                            ,varType=vVarType
                                                            ,varSourcePos
                                                            ,varScope = vScope
                                                            ,varInitValue = vInitVal
                                                            ,varArg
                                                            ,varConst=varConst' }
                                                         :vVars)

validateFunc :: Program SemExpr -> Func SynExpr -> Func SemExpr
validateFunc pr synFunc@UserFunc{uFuncName, uFuncStatments, uFuncRetType, uFuncPos} = func
    where
      func = if (vFuncType==VoidType) || (checkReturnPaths uFuncStatments)
             then UserFunc{uFuncName, uFuncArgs=vArgs, uFuncStatments=vStatments, uFuncRetType=vFuncType, uFuncPos}
             else throw $ ReturnPathsSemExc synFunc
      vFuncType = validateType uFuncRetType uFuncPos
      vArgs = reverse $ filter (\Var{varScope, varArg} -> varArg && (varScope==InFunction func)) $ programVars pr
      vStatments = map (validateStatment pr (InFunction func)) uFuncStatments

validateFunc _ _ = error "Buildin function validation"

validateStatment :: Program SemExpr -> ProgramPos SemExpr -> Statment SynExpr -> Statment SemExpr
validateStatment pr scope st@(IfSt ex sts pos) =
    let valEx = validateExpr pr scope ex
    in if (typeOfExpr valEx == BoolType)
       then IfSt valEx (map (validateStatment pr scope) sts) pos
       else throw $ NeedBoolSemExc st

validateStatment pr scope st@(IfElseSt ex ifSts elseSts pos) =
    let valEx = validateExpr pr scope ex
    in if (typeOfExpr valEx == BoolType)
       then IfElseSt valEx (map (validateStatment pr scope) ifSts)
                (map (validateStatment pr scope) elseSts) pos
       else throw $ NeedBoolSemExc st

validateStatment _ _ (BreakSt pos) = (BreakSt pos)

validateStatment _ scope (NextSt stateName pos) =
    case scope of
      (InState thread _) -> NextSt (stateByName thread stateName pos) pos
      _ -> throw $ NextNotInStateSemExc pos

validateStatment pr scope st@(WhileSt ex sts pos) =
    let valEx = validateExpr pr scope ex
    in if (typeOfExpr valEx == BoolType)
       then WhileSt valEx (map (validateStatment pr scope) sts) pos
       else throw $ NeedBoolSemExc st

validateStatment pr scope st@(WaitSt ex n pos) =
    let valEx = validateExpr pr scope ex
    in case typeOfExpr valEx of
         BoolType -> WaitSt valEx n pos
         TimeType -> WaitSt valEx n pos
         _ -> throw $ NeedBoolSemExc st

validateStatment pr scope st@(AssignSt rValue expr pos) =
    let valExpr = validateExpr pr scope expr
        valType = typeOfExpr valExpr
        rValue' = validateRValue pr scope pos rValue
        getRValueType (RValueVar Var{varType}) = varType
        getRValueType (RValueArrayAccess rv _) =
            case getRValueType rv of
              (ArrayType varType _) -> varType
              _ -> throw $ AssignWrongTypeSemExc st
        rValueType = getRValueType rValue'
    in if (valType `typeCanBe` rValueType)
       then AssignSt rValue' valExpr pos
       else throw $ AssignWrongTypeSemExc st

validateStatment pr scope (ReturnSt expr pos) =
    let vExpr = validateExpr pr scope expr
        ok = (case scope of
                InState{} -> voidExpr vExpr
                InFunction UserFunc{uFuncRetType} -> typeOfExpr vExpr `typeCanBe` uFuncRetType
                _ -> error "Cannot be retun here, wtf?")
    in if ok
       then ReturnSt vExpr pos
       else throw $ ReturnWrongTypeSemExc pos

validateStatment pr scope (FunSt e pos) = FunSt (validateExpr pr scope e) pos

validateRValue :: Program SemExpr -> ProgramPos SemExpr -> SourcePos  -> RValue SynExpr -> RValue SemExpr
validateRValue pr scope pos (RValueVar varName) = if not varConst then RValueVar var
                                                  else throw $ AssignConstVariableSemExc varName pos
    where var@Var{varConst} = varByName pr scope varName pos
validateRValue pr scope pos (RValueArrayAccess rValue expr) = RValueArrayAccess rValue' expr'
    where rValue' = validateRValue pr scope pos rValue
          expr' = validateExpr pr scope expr

validateExpr :: Program SemExpr -> ProgramPos SemExpr -> SynExpr -> SemExpr
validateExpr pr scope expr = validateExpr' pr scope $ dropSubExpr $ applyOpPriority expr
    where
      validateExpr' :: Program SemExpr -> ProgramPos SemExpr -> SynExpr -> SemExpr
      validateExpr' _ _ VoidSynExpr = VoidSemExpr
      validateExpr' _ _ (NumSynExpr n pos) = if n>(2^31) || n<(-(2^31))
                                             then throw $ IntTooBigSemExc n pos
                                             else NumSemExpr n
      validateExpr' _ _ (BoolSynExpr b) = BoolSemExpr b
      validateExpr' _ _ (StringSynExpr s) = StringSemExpr s
      validateExpr' _ _ (TimeSynExpr t) = TimeSemExpr t
      validateExpr' pr scope e@(ArrayAccessSynExpr pos expr index) = let expr' = validateExpr pr scope expr
                                                                         index' = validateExpr pr scope index
                                                                         unsubExpr (SubSemExpr e) = e
                                                                         unsubExpr e = e
                                                                         isArrayExpr (ArraySemExpr _ ) = True
                                                                         isArrayExpr _ = False in
                                                                     if (not.isArrayExpr.unsubExpr) expr' && (isArrayType $ typeOfExpr expr')
                                                                            && (typeOfExpr index' `typeCanBe` Int32Type)
                                                                     then ArrayAccessSemExpr expr' index'
                                                                     else throw $ NotArrayAccessSemExpr e pos
      validateExpr' pr scope (VarSynExpr varName pos) = VarSemExpr $ varByName pr scope varName pos
      validateExpr' pr scope (FunSynExpr exprFunc args pos) =
          let vArgs = map (validateExpr' pr scope) args
              func = funcByName pr exprFunc pos
              argTypes = funcArgsTypes func
          in if (length argTypes == length vArgs)
                 && (and $ zipWith typeCanBe (map typeOfExpr vArgs) argTypes)
             then FunSemExpr func vArgs
             else throw $ FuncCallWrongTypeSemExc func pos
      validateExpr' pr scope (SubSynExpr e) = SubSemExpr $ validateExpr' pr scope e
      validateExpr' pr scope exp@(OpSynExpr op lExpr rExpr _) =
          let vlExpr = validateExpr' pr scope lExpr
              vrExpr = validateExpr' pr scope rExpr
              vlType = typeOfExpr vlExpr
              vrType = typeOfExpr vrExpr
              (Just synOps) = Map.lookup op opTypes
              (semOp,(_,_,opRetType)) = let ops = filter (\(so, (lt, rt, _)) ->
                                                              (vlType `typeCanBe` lt)
                                                          && (vrType `typeCanBe` rt)
                                                          && (so `elem` synOps))
                                                  $ semOpTypes
                                        in if null ops
                                           then throw $ OpSemExc exp
                                           else head ops
          in OpSemExpr semOp vlExpr vrExpr opRetType
      validateExpr' pr scope (ArraySynExpr pos es) = ArraySemExpr es''
          where es' = map (validateExpr' pr scope) es
                headType = typeOfExpr $ head es'
                es'' = if (length es' > 0) && (all ((==headType).(typeOfExpr)) $ tail es')
                         then es'
                         else throw $ NotSameTypesInArraySemExc pos

validateType :: VarTypeName -> SourcePos -> VarType
validateType (VarTypeName typeName) pos = case Map.lookup (map toUpper typeName)
                                             $ Map.fromList
                                             $ [("INT8", Int8Type)
                                               ,("INT16", Int16Type)
                                               ,("INT32", Int32Type)
                                               ,("BOOL", BoolType)
                                               ,("STRING", StringType)
                                               ,("TIME", TimeType)
                                               ,("VOID", VoidType)
                                               ,("ЦЕЛ8", Int8Type)
                                               ,("ЦЕЛ16", Int16Type)
                                               ,("ЦЕЛ32", Int32Type)
                                               ,("ЛОГ", BoolType)
                                               ,("СТРОКА", StringType)
                                               ,("ВРЕМЯ", TimeType)
                                               ,("ПУСТО", VoidType)]
                                          of Nothing -> throw $ NoSuchTypeSemExc typeName pos
                                             Just vVarType -> vVarType
validateType (ArrayVarTypeName vt size) pos = ArrayType (validateType vt pos) size

-- ByName functions
threadByName :: Program SemExpr -> ThreadName -> SourcePos -> Thread SemExpr
threadByName ~Program{programThreads} (ThreadName thName) pos =
    let threads = filter (\Thread{threadName} -> threadName == thName) programThreads
    in if null threads
       then throw $ NoSuchThreadSemExc thName pos
       else head threads

stateByName :: Thread SemExpr -> StateName -> SourcePos -> ThreadState SemExpr
stateByName thread@Thread{threadName} (StateName stName) pos =
    let states = filter (\ThreadState{stateName} -> stateName == stName) $ threadStates thread
    in if null states
       then throw $ NoSuchStateSemExc threadName stName pos
       else head states

funcByName :: Program SemExpr -> FuncName -> SourcePos -> Func SemExpr
funcByName ~Program{programFuncs} fun@(FuncName fn) pos =
    let funcs = filter ((==fn).funcName) programFuncs
    in if null funcs
       then throw $ NoSuchFuncSemExc fun pos
       else head $ funcs

varByName :: Program SemExpr -> ProgramPos SemExpr -> VarName -> SourcePos -> Var SemExpr
varByName ~Program{programVars} scope varName pos =
    case varName of
      VarName vName -> let vars = filter (\Var{varName=n} -> vName == n)
                                 $ scopeVars scope programVars
                      in if null vars
                         then throw $ NoSuchVarSemExc varName pos
                         else head vars
      LongVarName thName vName -> let vars = filter (\Var{varName, varScope} ->
                                                    case varScope of
                                                      InThread Thread{threadName} -> (thName==threadName)
                                                                                    && (varName==vName)
                                                      _ -> False) programVars
                                 in if null vars
                                    then throw $ NoSuchVarSemExc varName pos
                                    else head vars

-- Aux fucntions
usedVars :: SemExpr -> [Var SemExpr]
usedVars (VarSemExpr var) = [var]
usedVars (OpSemExpr _ leftExpr rightExpr _ ) = (usedVars leftExpr) ++ (usedVars rightExpr)
usedVars (SubSemExpr subExpr) = usedVars subExpr
usedVars (FunSemExpr _ args) = concat $ map usedVars args
usedVars (ArraySemExpr vals) = concat $ map usedVars vals
usedVars _ = []

transScope :: Program SemExpr -> ProgramPos SynExpr -> SourcePos -> ProgramPos SemExpr
transScope _ InGlobal _= InGlobal
transScope pr (InFunction funcName) pos = InFunction $ funcByName pr funcName pos
transScope pr (InThread threadName) pos = InThread $ threadByName pr threadName pos
transScope pr (InState threadName stateName) pos =
    let thread = threadByName pr threadName pos
        state = stateByName thread stateName pos
    in InState thread state

scopeVars :: (Expression a) => ProgramPos a -> [Var a] -> [Var a]
scopeVars scope = filter (\Var{varScope} -> scope `posChildOf` varScope)

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
                ,DivSynOp
                ,ModSynOp]
               ]

applyOpPriority :: SynExpr -> SynExpr
applyOpPriority (SubSynExpr e) = SubSynExpr $ applyOpPriority e
applyOpPriority (FunSynExpr fun args pos) = FunSynExpr fun (map applyOpPriority args) pos
applyOpPriority oe@(OpSynExpr op le re pos) = if opExpr re
                                              then applyOpPriority' priorityList oe oe
                                              else OpSynExpr op (applyOpPriority le) (applyOpPriority re) pos
    where applyOpPriority' :: [[SynOper]] -> SynExpr -> SynExpr -> SynExpr
          applyOpPriority' [] exprHead (OpSynExpr eo le re pos) = (OpSynExpr eo le (applyOpPriority' [] exprHead re) pos)
          applyOpPriority' [] _ e = applyOpPriority e
          applyOpPriority' pl@(op:ops) exprHead oe@(OpSynExpr eo le re pos) =
              if eo `elem` op
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
          changeExprVar (OpSynExpr op le re pos) from to = OpSynExpr op le (if re == from
                                                                            then to
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
      dropSubExpr' (SubSynExpr se) = if constExpr se || varExpr se
                                     then se
                                     else SubSynExpr $ dropSubExpr' se
      dropSubExpr' (FunSynExpr fun args pos) = FunSynExpr fun (map dropSubExpr' args) pos
      dropSubExpr' se = se

checkReturnPaths :: (Expression e) => [Statment e] -> Bool
checkReturnPaths [] = False
checkReturnPaths (ReturnSt{}:_) = True
checkReturnPaths ((IfElseSt _ ifSts elseSts _):restSts) =
    checkReturnPaths restSts || (checkReturnPaths ifSts && checkReturnPaths elseSts)
checkReturnPaths (_:restSts) = checkReturnPaths restSts

duplicatesInList :: Eq a => [a] -> Maybe a
duplicatesInList l = duplicatesInList' l []
    where duplicatesInList' [] _ = Nothing
          duplicatesInList' (x:xs) rest = if x `elem` rest
                                          then Just x
                                          else duplicatesInList' xs (x:rest)