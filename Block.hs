module Block where

import AbsCinnabar
import {-# SOURCE #-} PState
import {-# SOURCE #-} Statement

runBlock :: Block -> PSt -> SCont -> ECont -> Result
runBlock (SBlock stmts) st cont retCont = foldr bindCont cont stmts st where
    bindCont stmt c st1 = runStatement stmt st1 c retCont
