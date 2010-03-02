{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Validator(validateProgram)
where
import Language.Rfx.Structures
import qualified Data.Map as Map

validateProgram :: Program SynExpr -> Program SemExpr
validateProgram Program{programThreads, programVars} = Program{programThreads=[], programVars=[]}
    -- let validatedVars = validateVars programVars in 
    -- Program{programThreads = map (validateThread validatedVars) programThreads , programVars=[]}

validateThread :: [Var SynExpr] -> Thread SynExpr -> Thread SemExpr
validateThread vars Thread{threadName, threadStates} =
    Thread{threadName, threadStates = map (validateState vars) threadStates}

validateState :: [Var SynExpr] -> ThreadState SynExpr -> ThreadState SemExpr
validateState vars ThreadState{stateName, stateStatments} =
    ThreadState{stateName, stateStatments = map (validateStatment vars) stateStatments}

validateStatment :: [Var SynExpr] -> Statment SynExpr -> Statment SemExpr
validateStatment _ _ = BreakSt

validateVars :: Program SemExpr -> [Var SynExpr] -> [Var SemExpr]
validateVars pr vars = validateVars' vars [] where
    validateVars' :: [Var SynExpr] -> [Var SemExpr] -> [Var SemExpr]
    validateVars' [] vVars = vVars
    validateVars' (var@Var{varScope, varInitValue,
                           varType, varName}:uVars) vVars = let vInitVal = validateExpr pr InGlobal varInitValue
                                                                vScope = transScope pr varScope in
                                                            if (null $ usedVars vInitVal)
                                                            && (typeOfExpr pr InGlobal vInitVal == varType)
                                                            && (null $ filter (\Var{varName=vn} -> vn == varName) $ scopeVars vScope vVars)
                                                        then validateVars' uVars (Var{varName
                                                                                     ,varType
                                                                                     ,varScope = vScope 
                                                                                     ,varInitValue = vInitVal}                                                                                                 
                                                                                  :vVars)
                                                        else error "Var validate error"
                    
usedVars :: SemExpr -> [Var SemExpr]
usedVars (VarSemExpr var) = [var]
usedVars (OpSemExpr _ leftExpr rightExpr) = (usedVars leftExpr) ++ (usedVars rightExpr)
usedVars (SubSemExpr subExpr) = usedVars subExpr
usedVars (FunSemExpr ()) = [] -- TODO
usedVars _ = []

typeOfExpr :: (Expression a) => Program a -> ProgramPos a -> a -> VarType
typeOfExpr _ _ _ = Int8Type
             
scopeVars :: (Expression a) => ProgramPos a -> [Var a] -> [Var a]
scopeVars scope = filter (\Var{varScope} -> varScope `posChildOf` scope)

transScope :: Program SemExpr -> ProgramPos SynExpr -> ProgramPos SemExpr
transScope _ InGlobal = InGlobal
transScope pr (InThread Thread{threadName}) = InThread $ threadByName pr threadName
transScope pr (InState Thread{threadName} ThreadState{stateName}) =
    InState (threadByName pr threadName) (stateByName pr threadName stateName)
                  
threadByName :: Program SemExpr -> String -> Thread SemExpr
threadByName Program{programThreads} thName = head $ filter (\Thread{threadName} -> threadName == thName) programThreads

stateByName :: Program SemExpr -> String -> String -> ThreadState SemExpr                                              
stateByName pr thName stName = head $ filter (\ThreadState{stateName} -> stateName == stName) $ threadStates $ threadByName pr thName
                  
varByName :: Program SemExpr -> ProgramPos SemExpr -> VarName -> Var SemExpr
varByName pr@Program{programVars} scope varName = case varName of
                                                 VarName vName -> head $ filter (\Var{varName=n} -> vName == n)
                                                       $ scopeVars scope programVars
                                                 LongVarName thName vName -> head $ filter (\Var{varName=n} -> vName == n)
                                                       $ scopeVars (InThread $ threadByName pr thName) programVars
                  
validateExpr :: Program SemExpr -> ProgramPos SemExpr -> SynExpr -> SemExpr
validateExpr pr _ (NumSynExpr n) = NumSemExpr n
validateExpr pr _ (StringSynExpr s) = StringSemExpr s
validateExpr pr scope (VarSynExpr varName) = VarSemExpr $ varByName pr scope varName
validateExpr pr _ (FunSynExpr _ _ ) = FunSemExpr () -- TODO
-- validateExpr pr scope (OpSynExpr op lExpr rExpr) = let vlExpr = validateExpr pr scope lExpr
--                                                        vrExpr = validateExpr pr scope rExpr
--                                                        vlType = typeOfExpr pr scope vlExpr
--                                                        vrType = typeOfExpr pr scope vrExpr
--                                                        (Just synOps) = Map.lookup op opTypes 
--                                                    in
                                                     

                                             
opTypes :: Map.Map SynOper [SemOper]
opTypes = Map.fromList [(PlusSynOp, [])
                       ,(MinusSynOp, [])
                       ,(DivSynOp, [])
                       ,(EqualitySynOp, [])
                       ,(GrSynOp, [])
                       ,(LsSynOp, [])
                       ,(GrEqSynOp, [])
                       ,(LsEqSynOp, [])]

semOpTypes :: Map.Map SemOper (VarType, VarType, VarType)
semOpTypes = Map.fromList [ (NumPlusSynOp, (Int8Type, Int8Type, Int8Type))
                           ,(StringPlusSynOp, (StringType, StringType, StringType))
                           ,(NumMinusSynOp, (Int8Type, Int8Type, Int8Type))
                           ,(NumMulSynOp, (Int8Type, Int8Type, Int8Type))]
                                  