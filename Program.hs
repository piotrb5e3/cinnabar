module Program where

import Control.Monad.Trans.State

import AbsCinnabar
import PState
import Statement

runProgram :: Program -> PSt -> Result
runProgram (Prog stmts) = foldr bindCont (const ("", "", False)) stmts where
    bindCont stmt c st1 = runStatement stmt st1 c retCont
    retCont _ _ = showError "Unexpected return"
