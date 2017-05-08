module Statement where
import qualified Data.Map.Strict as M

import AbsCinnabar
import {-# SOURCE #-} PState
import Expression
import Block

runStatement :: Stmt -> PSt -> SCont -> ECont -> Result
runStatement (SWhile e b) st cont retCont = c0 st where
  c0 st1 = evalExpr e st1 c1 where
    c1 ref st2 = truthHelper ref st2 c2 cont where
      c2 st3 = runBlock b st3 c0 retCont

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
        c1 ref2 st3 = case unCharListRef st3 ref2 of
          Just str -> writeStdout str st3 cont

runStatement (SAssert e) st cont retCont = evalExpr e st c0 where
  c0 ref st2 = truthHelper ref st2 cont assertErr
  assertErr = const $ showError "Assertion failed!"

runStatement (SExpr e) st cont retCont = evalExpr e st $ const cont

assignRefToLVal :: LVal -> VRef -> PSt -> SCont -> Result
assignRefToLVal lv ref st cont = case lv of

    ATuple lvals -> case ref2val st ref of
      L rl -> if length rl < length lvals
        then showError ("Not enough members to unpack. Expected at least " ++ show (length lvals) ++ ", got " ++ show (length rl))
        else foldr (\(llv, rref) cont2 st2 -> assignRefToLVal llv rref st2 cont2) cont (zip lvals rl) st
      _ -> showError "Only list values can be unpacked"

    AAt e0 e1 -> eval2Expr e0 e1 st c0 where
        c0 ref1 ref2 st2 = case (ref2val st2 ref1, ref2val st2 ref2) of
          (L lrefs, I i) -> if i < 0 || i >= length lrefs
            then showError $ "List index out of range: " ++ show i
            else setStoreValue ref1 (L lrefs2) st2 cont where
              lrefs2 = lrf ++ ref:lrt
              (lrf, _:lrt) = splitAt i lrefs
          (L _, _) -> showError "Lists are only integer-subscriptable"
          (O _, L []) -> showError "Object member names must be non-empty strings"
          (O m, L lr) -> case unCharList st2 lr of
            Just str -> objectSet ref1 str ref st2 cont
            Nothing -> showError "Object member names must be non-empty strings"
          (O _, _) -> showError "Object member names must be non-empty strings"
          (D m, _) -> dictSet ref1 ref2 ref st2 cont
          _ -> showError "Only list, dictionary and object values can be subscripted"
    AMember e (Ident mid) -> evalExpr e st c0 where
      c0 ref2 st2 = case ref2val st2 ref2 of
        O m -> objectSet ref2 mid ref st2 cont
        _ -> showError ("Cannot assign to value's member named " ++ mid)

    AVar (Ident str) -> setVarRef str ref st cont

