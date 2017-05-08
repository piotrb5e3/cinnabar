module Program where

import Control.Monad.Trans.State

import AbsCinnabar
import StateTypes
import StateModifiers
import Statement

runProgram :: Program -> PSt -> Result
runProgram (Prog stmts) = foldr bindCont (const ("", "", False)) stmts where
    bindCont stmt c = runStatement stmt c retCont
    retCont _ _ = showError "Unexpected return"
