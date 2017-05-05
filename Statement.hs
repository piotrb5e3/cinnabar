module Statement where
import qualified Data.Map.Strict as M

import AbsCinnabar
import {-# SOURCE #-} PState
import Expression
import Block

runStatement :: Stmt -> PSt -> SCont -> ECont -> Result
runStatement (SWhile e b) st cont retCont = evalExpr e st c0 where
  c0 ref st2 = truthHelper ref st2 c1 cont where
    c1 st3 = runBlock b st3 c2 retCont where
       c2 st4 = runStatement (SWhile e b) st4 cont retCont

runStatement (SCond e b) st cont retCont = evalExpr e st c0 where
    c0 ref st2 = truthHelper ref st2 c1 cont where
        c1 st3 = runBlock b st3 cont retCont

runStatement (SCondElse e tb fb) st cont retCont = evalExpr e st c0 where
    c0 ref st2 = truthHelper ref st2 tc fc where
        tc st3 = runBlock tb st3 cont retCont
        fc st3 = runBlock fb st3 cont retCont

runStatement (SAssing l e) st cont retCont = evalExpr e st c0 where
  c0 ref st2 = assignRefToLVal l ref st2 cont

runStatement (SReturn e) st cont retCont = evalExpr e st retCont

runStatement (SPrint e) st cont retCont = evalExpr e st c0 where 
    c0 ref st2 = toStrHelper False ref st2 c1 where
        c1 ref2 st3 = case unCharList st3 ref2 of
          Just str -> writeStdout str st3 cont
          Nothing -> showError "toStrHelper returned a non-string element!"

runStatement (SAssert e) st cont retCont = evalExpr e st c0 where
  c0 ref st2 = truthHelper ref st2 cont assertErr
  assertErr = const $ showError "Assertion failed!"

runStatement (SExpr e) st cont retCont = evalExpr e st $ \ref st2 -> cont st2

assignRefToLVal :: LVal -> VRef -> PSt -> SCont -> Result
assignRefToLVal lv ref st cont = case lv of
    ATuple lvals -> case store st M.! ref of
      L rl -> if length rl < length lvals
        then showError ("Not enough members to unpack. Expected at least " ++ show (length lvals) ++ ", got " ++ show (length rl))
        else foldr (\(llv, rref) cont2 st2 -> assignRefToLVal llv rref st2 cont2) cont (zip lvals rl) st
      _ -> showError "Only list values can be unpacked"
    AAt e0 e1 -> evalExpr e0 st c0 where
      c0 ref1 st2 = evalExpr e1 st2 c1 where
        c1 ref2 st3 = case (store st3 M.! ref1, store st3 M.! ref2) of
          (L lrefs, I i) -> if i < 0 || i >= length lrefs
            then showError $ "List index out of range: " ++ show i
            else  setStoreValue ref1 (L lrefs2) st3 cont where
              lrefs2 = lrf ++ ref:lrt
              (lrf, _:lrt) = splitAt i lrefs
          (L _, _) -> showError "Lists are only integer-subscriptable"
          (O _, _) -> showError "Not implemented yet"
          (D _, _) -> showError "Not implemented yet"
          _ -> showError "Only list, dictionary and object values can be subscripted"
    AMember e mid -> showError "Not implemented yet"
    AVar (Ident str) -> setVarRef str ref st cont
