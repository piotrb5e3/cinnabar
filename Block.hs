module Block where

import AbsCinnabar
import StateTypes
import {-# SOURCE #-} Statement

runBlock :: Block -> SCont -> ECont -> PSt -> Result
runBlock (SBlock stmts) cont retCont = foldr bindCont cont stmts where
  bindCont stmt c = runStatement stmt c retCont
