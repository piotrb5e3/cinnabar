module Expression where
import qualified Data.Map.Strict as M

import AbsCinnabar
import PState
import Values

evalExpr :: Expr -> PSt -> ECont -> Result
evalExpr (ELambda ids e) st cont = cont (R 0) st --FIXME
evalExpr (EFun ids b) st cont = cont (R 0) st --FIXME
evalExpr (EIf ec et ef)st cont = cont (R 0) st --FIXME
evalExpr (EOr e0 e1) st cont = cont (R 0) st --FIXME
evalExpr (EAnd e0 e1) st cont = cont (R 0) st --FIXME
evalExpr (ERel e0 op e1) st cont = cont (R 0) st --FIXME
evalExpr (EAdd e0 op e1) st cont = cont (R 0) st --FIXME
evalExpr (EMul e0 op e1) st cont = cont (R 0) st --FIXME
evalExpr (EPow e0 e1) st cont = cont (R 0) st --FIXME
evalExpr (ENot e) st cont = cont (R 0) st --FIXME
evalExpr (ENeg e) st cont = cont (R 0) st --FIXME
evalExpr (ECall e el) st cont = cont (R 0) st --FIXME
evalExpr (EMember e mid) st cont = cont (R 0) st --FIXME
evalExpr (EAt e0 e1) st cont = cont (R 0) st --FIXME
evalExpr (EExtend e0 e1) st cont = cont (R 0) st --FIXME
evalExpr (ENew e el) st cont = cont (R 0) st --FIXME
evalExpr (EChar c) st cont = cont (R 0) st --FIXME
evalExpr (EString s) st cont = charList s st cont
evalExpr (ELitInt i) st cont = cont (R 0) st --FIXME
evalExpr ELitTrue st cont = cont (R 0) st --FIXME
evalExpr ELitFalse st cont = cont (R 0) st --FIXME
evalExpr (EVar vid) st cont = cont (R 0) st --FIXME
evalExpr (EList el) st cont = cont (R 0) st --FIXME
evalExpr (EListComp e lv eit) st cont = cont (R 0) st --FIXME
evalExpr (EDict ldm) st cont = cont (R 0) st --FIXME

truthHelper :: Value -> PSt -> SCont -> SCont -> Result
truthHelper (B True) st tcont fcont = tcont st
truthHelper (B False) st tcont fcont = fcont st
truthHelper _ st tcont fcont = showError "Only Boolean values can be tested for truth"

toStrHelper :: VRef -> PSt -> ECont -> Result
toStrHelper ref st cont = case store st M.! ref of
  I i -> charList (show i) st cont
  C c -> charList ['\'', c, '\''] st cont
  L els -> if all (isCharRef st) els then cont ref st
              else cont (R 0) st

charList :: String -> PSt -> ECont -> Result
charList str st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (L []) st2 c1 where
    c1 st3 = foldr bindCont (cont ref) str st3 where
    bindCont chr cont1 st4 = charVal chr st4 bindAppend where
      bindAppend ref2 s5 = listAppend ref ref2 s5 cont1

charVal :: Char -> PSt -> ECont -> Result
charVal c st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (C c) st2 $ cont ref

listAppend :: VRef -> VRef -> PSt -> SCont -> Result
listAppend lref aref st cont = case store st M.! lref of
  L elems -> setStoreValue lref (L $ elems ++ [aref]) st cont
  _       -> showError "Not a list"

unCharList ::  PSt -> VRef -> Maybe String
unCharList st ref = case store st M.! ref of
  L els -> mapM (unValChar st) els
  _ -> Nothing

unValChar :: PSt -> VRef -> Maybe Char
unValChar st ref = case store st M.! ref of
  C c -> Just c
  _   -> Nothing

isCharRef :: PSt -> VRef -> Bool
isCharRef st ref = case store st M.! ref of
  C _ -> True
  _   -> False
