{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Compiler.Common
where
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Rfx.Structures

data CompilerTarget = PIC | AVR8 | AVR16 | AVR32 | ARM | ANSI deriving (Eq, Show)
data CompilerLanguage = C | Dot | ST | Python deriving (Eq, Show)
data CompilerOptions = CompilerOptions
                     {
                       compilerTarget :: CompilerTarget
                     , compilerLanguage :: CompilerLanguage
                     } deriving (Eq, Show)

data CompilerState = CompilerState
                   {
                     compilerCode :: String
                   , compilerOptions :: CompilerOptions
                   , compilerIterators :: Map.Map String Int
                   , compilerIndent :: Int
                   , compilerVars :: Set.Set (Var SemExpr)
                   , compilerCurrentThread :: Thread SemExpr
                   , compilerCurrentState :: ThreadState SemExpr
                   } deriving (Eq, Show)

type Compiler a = State CompilerState a

addString :: String -> Compiler ()
addString s = state (\state -> ((), state{compilerCode=(compilerCode state)++s}))

dropLastChar ::Compiler ()
dropLastChar = state (\state -> ((), state{compilerCode = init (compilerCode state)}))

makeIndent :: Compiler ()
makeIndent = state (\state -> ((), state{compilerCode=(compilerCode state)
                                          ++ (replicate (compilerIndent state) ' ')}))

withIndent :: Compiler () -> Compiler ()
withIndent compiler = do
  addIndent
  compiler
  subIndent

addLine :: String -> Compiler ()
addLine s = do
  makeIndent
  addString s
  addString "\n"

nextIterator :: String -> Compiler Int
nextIterator s = state (\state ->
                            let its = compilerIterators state in
                            if Map.member s its then
                                (its Map.! s, state{compilerIterators=
                                                        Map.update (Just . (+1)) s its})
                            else
                                (0, state{compilerIterators=
                                              Map.insert s 0 its}))

zeroIterator :: String -> Compiler ()
zeroIterator s = state (\state ->
                            let its = compilerIterators state in
                            ((), state{compilerIterators=
                                           (if Map.member s its then
                                                Map.update (const $ Just 0) s its
                                            else
                                                Map.insert s 0 its)}))

addIndent :: Compiler ()
addIndent = state (\state -> ((), state{compilerIndent=(compilerIndent state)+4}))

subIndent :: Compiler ()
subIndent = state (\state -> ((), state{compilerIndent=(compilerIndent state)-4}))

getVarsFromScope :: ProgramPos SemExpr -> Compiler [Var SemExpr]
getVarsFromScope pos = state (\state ->
                               (Set.toList $ Set.filter (\ var -> (varScope var) == pos) (compilerVars state),
                                state))

enterThread :: Thread SemExpr -> Compiler ()
enterThread th =state (\state -> ((),state{compilerCurrentThread = th}))

getCurrentThread :: Compiler (Thread SemExpr)
getCurrentThread = state (\state -> ((compilerCurrentThread state), state))

getCurrentState :: Compiler (ThreadState SemExpr)
getCurrentState = state (\state -> ((compilerCurrentState state), state))

enterState :: ThreadState SemExpr -> Compiler ()
enterState st = state (\state -> ((), state{compilerCurrentState=st}))

getState :: String -> Compiler (ThreadState SemExpr)
getState stName = do
  curThread <- getCurrentThread
  let states = filter (\st -> (stateName st) == stName) (threadStates curThread)
  case length states of
    1 -> return $ head states
    _ -> return $ error $ "No such state " ++ stName
