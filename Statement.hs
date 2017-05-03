module Statement where
import qualified Data.Map.Strict as M

import AbsCinnabar
import PState
import Values
import Expression

runStatement :: Stmt -> PSt -> SCont -> ECont -> Result
runStatement (SWhile e b) st cont retCont = cont st -- FIXME
runStatement (SCond e b) st cont retCont = evalExpr e st c0 where
    c0 ref st2 = truthHelper val st2 c1 cont where
        c1 st3 = runBlock b st3 cont retCont
        val = store st2 M.! ref
runStatement (SCondElse e tb fb) st cont retCont = evalExpr e st c0 where
    c0 ref st2 = truthHelper val st2 tc fc where
        tc st3 = runBlock tb st3 cont retCont
        fc st3 = runBlock fb st3 cont retCont
        val = store st2 M.! ref
runStatement (SAssing l e) st cont retCont = cont st -- FIXME
runStatement (SReturn e) st cont retCont = evalExpr e st retCont
runStatement (SPrint e) st cont retCont = evalExpr e st c0 where 
    c0 ref st2 = toStrHelper ref st2 c1 where
        c1 ref2 st3 = case unCharList st3 ref2 of
          Just str -> writeStdout str st3 cont
          Nothing -> showError "toStrHelper returned a non-string element!"
runStatement (SAssert e) st cont retCont = cont st -- FIXME
runStatement (SExpr e) st cont retCont = evalExpr e st $ \ref st2 -> cont st2

runBlock :: Block -> PSt -> SCont -> ECont -> Result
runBlock (SBlock stmts) st cont retCont = foldr bindCont cont stmts st where
    bindCont stmt c st1 = runStatement stmt st1 c retCont
