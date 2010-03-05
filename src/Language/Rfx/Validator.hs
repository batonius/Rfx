{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Validator(validateProgram)
where
import Language.Rfx.Error
import Language.Rfx.Structures
import qualified Data.Map as Map
import Control.Exception hiding (try)
    
validateProgram :: Program SynExpr -> Program SemExpr
validateProgram Program{programThreads=prThs, programVars=prVrs} = program'
    where program = Program{programVars=validateVars program prVrs
                           ,programThreads=map (validateThread program) prThs}
          program' = Program{programThreads=(programThreads program),
                             programVars=replaceVars program (programVars program)}

replaceVars :: Program SemExpr -> [Var SemExpr] -> [Var SemExpr]
replaceVars pr [] = []
replaceVars pr (var@(Var{varScope}):vars) = var{varScope=trScope pr varScope} : (replaceVars pr vars)

trScope pr InGlobal = InGlobal
trScope pr (InThread Thread{threadName}) = InThread $ threadByName pr threadName
trScope pr (InState Thread{threadName} ThreadState{stateName}) =
    InState (threadByName pr threadName) (stateByName pr threadName stateName)
    
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
                           varType, varName, varSourcePos}:uVars) vVars =
      let pr' = Program{programThreads=[], programVars=vVars}
          vInitVal = validateExpr pr' InGlobal varInitValue
          vScope = transScope pr varScope
      in if (not.null) $ usedVars vInitVal
         then throw $ VarInVarInitSemExc var
         else if (typeOfExpr pr' InGlobal vInitVal /= varType)
              then throw $ VarInitWrongTypeSemExc var
              else if (not.null) $ filter (\Var{varName=vn} -> vn == varName) $
                   scopeVars vScope vVars
                   then throw $ VarAlreadyExistsSemExc var
                   else validateVars' uVars (Var{varName
                                                ,varType
                                                ,varSourcePos
                                                ,varScope = vScope 
                                                ,varInitValue = vInitVal}                                                                                                 
                                             :vVars)
                    
usedVars :: SemExpr -> [Var SemExpr]
usedVars (VarSemExpr var) = [var]
usedVars (OpSemExpr _ leftExpr rightExpr) = (usedVars leftExpr) ++ (usedVars rightExpr)
usedVars (SubSemExpr subExpr) = usedVars subExpr
usedVars (FunSemExpr ()) = [] -- TODO
usedVars _ = []

typeOfExpr :: Program SemExpr -> ProgramPos SemExpr -> SemExpr -> VarType
typeOfExpr _ _ (NumSemExpr _) = Int8Type
typeOfExpr _ _ (VarSemExpr Var{varType}) = varType
typeOfExpr pr scope (SubSemExpr se) = typeOfExpr pr scope se
typeOfExpr _ _ (StringSemExpr _) = StringType
typeOfExpr _ _ (OpSemExpr semOp _ _) = opType
    where (_ , (_, _, opType)) = let ops = filter ((==semOp).fst) semOpTypes
                                 in if null ops
                                    then error $ "No such oper" ++ (show semOp)
                                    else head ops
             
scopeVars :: (Expression a) => ProgramPos a -> [Var a] -> [Var a]
scopeVars scope = filter (\Var{varScope} -> scope `posChildOf` varScope)

transScope :: Program SemExpr -> ProgramPos SynExpr -> ProgramPos SemExpr
transScope _ InGlobal = InGlobal
transScope pr (InThread Thread{threadName}) = InThread $ Thread {threadName, threadStates=[]}
transScope pr (InState Thread{threadName} ThreadState{stateName}) =
    InState (Thread{threadName, threadStates=[]}) (ThreadState {stateName, stateStatments=[]})
                  
threadByName :: Program SemExpr -> String -> Thread SemExpr
threadByName ~Program{programThreads} thName = head $ filter (\Thread{threadName} -> threadName == thName) programThreads

stateByName :: Program SemExpr -> String -> String -> ThreadState SemExpr                                              
stateByName pr thName stName = head $ filter (\ThreadState{stateName} -> stateName == stName) $ threadStates $ threadByName pr thName
                  
varByName :: Program SemExpr -> ProgramPos SemExpr -> VarName -> Var SemExpr
varByName ~pr@Program{programVars} scope varName = case varName of
                                                 VarName vName -> let vars = filter (\Var{varName=n} -> vName == n)
                                                                            $ scopeVars scope programVars
                                                                 in if null vars
                                                                    then error "No such var"
                                                                    else head vars
                                                 LongVarName thName vName -> head $ filter (\Var{varName=n} -> vName == n)
                                                       $ scopeVars (InThread $ threadByName pr thName) programVars
                  
validateExpr :: Program SemExpr -> ProgramPos SemExpr -> SynExpr -> SemExpr
validateExpr pr _ (NumSynExpr n) = NumSemExpr n
validateExpr pr _ (StringSynExpr s) = StringSemExpr s
validateExpr pr scope (VarSynExpr varName) = VarSemExpr $ varByName pr scope varName
validateExpr pr _ (FunSynExpr _ _ ) = FunSemExpr () -- TODO
validateExpr pr scope (SubSynExpr e) = SubSemExpr $ validateExpr pr scope e
validateExpr pr scope exp@(OpSynExpr op lExpr rExpr) = let vlExpr = validateExpr pr scope lExpr
                                                           vrExpr = validateExpr pr scope rExpr
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
                               
validateStatment pr ~(InState Thread{threadName} _) (NextSt (ThreadState{stateName})) = NextSt $ stateByName pr threadName stateName
                                                                                        
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
                                                  
