module Statement where
import qualified Data.Map.Strict as M

import AbsCinnabar
import StateTypes
import StateModifiers
import Expression
import Block

runStatement :: Stmt -> SCont -> ECont -> PSt -> Result
runStatement (SWhile e b) cont retCont = c0 where
  c0 = evalExpr e c1
  c1 ref = truthHelper ref c2 cont
  c2 = runBlock b c0 retCont

runStatement (SCond e b) cont retCont = evalExpr e c0 where
  c0 ref = truthHelper ref c1 cont
  c1 = runBlock b cont retCont

runStatement (SCondElse e tb fb) cont retCont = evalExpr e c0 where
    c0 ref = truthHelper ref tc fc
    tc = runBlock tb cont retCont
    fc = runBlock fb cont retCont

runStatement (SAssing l e) cont retCont = evalExpr e c0 where
  c0 ref = assignRefToLVal l ref cont

runStatement (SReturn e) cont retCont = evalExpr e retCont

runStatement (SPrint e) cont retCont = evalExpr e c0 where 
    c0 ref = toStrHelper False ref c1 where
        c1 ref2 st3 = case unCharListRef st3 ref2 of
          Just str -> writeStdout str cont st3

runStatement (SAssert e) cont retCont = evalExpr e c0 where
  c0 ref = truthHelper ref cont assertErr
  assertErr = const $ showError "Assertion failed!"

runStatement (SExpr e) cont retCont = evalExpr e $ const cont

assignRefToLVal :: LVal -> VRef -> SCont -> PSt -> Result
assignRefToLVal lv ref cont st = case lv of

    ATuple lvals -> case ref2val st ref of
      L rl -> if length rl < length lvals
        then showError ("Not enough members to unpack. Expected at least " ++ show (length lvals) ++ ", got " ++ show (length rl))
        else foldr (uncurry assignRefToLVal) cont (zip lvals rl) st
      _ -> showError "Only list values can be unpacked"

    AAt e0 e1 -> eval2Expr e0 e1 c0 st where
        c0 ref1 ref2 st2 = case (ref2val st2 ref1, ref2val st2 ref2) of
          (L lrefs, I i) -> if i < 0 || i >= length lrefs
            then showError $ "List index out of range: " ++ show i
            else setStoreValue ref1 (L lrefs2) cont st2 where
              lrefs2 = lrf ++ ref:lrt
              (lrf, _:lrt) = splitAt i lrefs
          (L _, _) -> showError "Lists are only integer-subscriptable"
          (O _, L []) -> showError "Object member names must be non-empty strings"
          (O m, L lr) -> case unCharList st2 lr of
            Just str -> objectSet ref1 str ref cont st2
            Nothing -> showError "Object member names must be non-empty strings"
          (O _, _) -> showError "Object member names must be non-empty strings"
          (D m, _) -> dictSet ref1 ref2 ref cont st2
          _ -> showError "Only list, dictionary and object values can be subscripted"
    AMember e (Ident mid) -> evalExpr e c0 st where
      c0 ref2 st2 = case ref2val st2 ref2 of
        O m -> objectSet ref2 mid ref cont st2
        _ -> showError ("Cannot assign to value's member named " ++ mid)

    AVar (Ident str) -> setVarRef str ref cont st

