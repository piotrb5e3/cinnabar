module Expression where
import Data.List (intercalate)
import qualified Data.Map.Strict as M

import AbsCinnabar
import PState
import Block

evalExpr :: Expr -> PSt -> ECont -> Result
evalExpr (ELambda ids e) st cont = allocAndSet (F (length ids) c0) st cont where
  c0 refs st2 cont2 = evalExpr e st3 c1 where
    st3 = PSt (store st2) (nextRef st2) newVars (input st2) where
      newVars = M.union (M.fromList (zip (map idToStr ids) refs)) $ vars st
    c1 ref st4 = cont2 ref $ PSt (store st4) (nextRef st4) (vars st2) (input st4)

evalExpr (EFun ids b) st cont = allocAndSet (F (length ids) c0) st cont where
  c0 refs st2 cont2 = runBlock b st3 sc0 c1 where
    st3 = PSt (store st2) (nextRef st2) newVars (input st2) where
      newVars = M.union (M.fromList (zip (map idToStr ids) refs)) $ vars st
    sc0 st5 = allocAndSet (I 0) st5 c1
    c1 ref st4 = cont2 ref $ PSt (store st4) (nextRef st4) (vars st2) (input st4)

evalExpr (EIf ec et ef) st cont = evalExpr ec st c0 where
  c0 ref st2 = truthHelper ref st2 tc fc where
    tc st3 = evalExpr et st3 cont
    fc st4 = evalExpr ef st4 cont

evalExpr (EOr e0 e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = truthHelper ref st2 (cont ref) c1 where
    c1 st3 = evalExpr e1 st3 c2 where
      c2 ref2 st4 = truthHelper ref2 st4 (cont ref2) (cont ref2) 

evalExpr (EAnd e0 e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = truthHelper ref st2 c1 (cont ref) where
    c1 st3 = evalExpr e1 st3 c2 where
      c2 ref2 st4 = truthHelper ref2 st4 (cont ref2) (cont ref2) 

evalExpr (ERel e0 op e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = evalExpr e1 st2 (crel ref)
  crel ref ref2 st4 = case (store st4 M.! ref, op, store st4 M.! ref2) of
    (I i1, _, I i2) -> allocAndSet (B (relOpFun op i1 i2)) st4 cont
    (C c1, _, C c2) -> allocAndSet (B (relOpFun op c1 c2)) st4 cont
    (B b1, Eq, B b2) -> allocAndSet (B (b1 == b2)) st4 cont
    (B b1, Ne, B b2) -> allocAndSet (B (b1 /= b2)) st4 cont
    _ -> showError "Cannot compare"

evalExpr (EAdd e0 op e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = evalExpr e1 st2 (cadd ref)
  cadd ref ref2 st4 = case (store st4 M.! ref, op, store st4 M.! ref2) of
    (I i1, Add, I i2) -> allocAndSet (I (i1 + i2)) st4 cont
    (I i1, Sub, I i2) -> allocAndSet (I (i1 - i2)) st4 cont
    (L refs, Add, L refs2 ) -> allocAndSet (L (refs ++ refs2)) st4 cont
    (_, Add, _) -> showError "Cannot add"
    (_, Sub, _) -> showError "Cannot substract"

evalExpr (EMul e0 op e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = evalExpr e1 st2 (cmul ref)
  cmul ref ref2 st4 = case (store st4 M.! ref, op, store st4 M.! ref2) of
    (I i1, Div, I 0) -> showError "Cannot divide by 0"
    (I i1, Mod, I 0) -> showError "Cannot divide by 0"
    (I i1, _, I i2) -> allocAndSet (I (mulOpFun op i1 i2)) st4 cont
    (L refs, Mul, I i ) -> allocAndSet (L ([1..i] >> refs)) st4 cont
    (_, Mul, _) -> showError "Cannot multiply"
    (_, Div, _) -> showError "Cannot divide"
    (_, Mod, _) -> showError "Cannot take modulo"

evalExpr (EPow e0 e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = evalExpr e1 st2 (cpow ref)
  cpow ref ref2 st4 = case (store st4 M.! ref, store st4 M.! ref2) of
    (I i1, I i2) -> allocAndSet (I (i1 ^ i2)) st4 cont
    _ -> showError "Cannot exponentiate"

evalExpr (ENot e) st cont = evalExpr e st c0 where
  c0 ref st2 = case store st2 M.! ref of
    B b -> allocAndSet (B (not b)) st2 cont
    _   -> showError "Only boolean values can be negated"

evalExpr (ENeg e) st cont = evalExpr e st c0 where
  c0 ref st2 = case store st2 M.! ref of
    I i -> allocAndSet (I (-i)) st2 cont
    _   -> showError "Only integer values can be inverted"

evalExpr (ECall e el) st cont = evalExpr e st c0 where
  c0 ref st2 = case store st2 M.! ref of
    F argc funC -> if length el == argc
      then evalExpr (EList el) st2 c1
      else showError ("Wrong number of parameters. Expected: " ++ show argc ++ " was: " ++ show (length el)) where
        c1 ref2 st3 = case store st3 M.! ref2 of
          L rl -> funC rl st3 cont
          _ -> showError "Internal error"
    _ -> showError "Called a non-callable value"

evalExpr (EMember e mid) st cont = showError "Not implemented yet"

evalExpr (EAt e0 e1) st cont = evalExpr e0 st c0 where
  c0 ref st2 = evalExpr e1 st2 c1 where
    c1 ref2 st3 = case (store st3 M.! ref, store st3 M.! ref2) of
      (L eRefs, I i) -> if i >= 0 && i < length eRefs
        then cont (eRefs !! i) st3
        else showError $ "List index out of range: " ++ show i
      (O m, vRef) -> showError "Not implemented yet"
      (D m, vRef) -> showError "Not implemented yet"
      (L _, _) -> showError "Bad list subscript type"
      _ -> showError "Type cannot be subscripted"

evalExpr (EExtend e0 e1) st cont = showError "Not implemented yet"

evalExpr (ENew e el) st cont = showError "Not implemented yet"

evalExpr (EChar c) st cont = charVal c st cont

evalExpr (EString s) st cont = charList s st cont

evalExpr (ELitInt i) st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (I (fromIntegral i)) st2 (cont ref)

evalExpr ELitTrue st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (B True) st2 (cont ref)

evalExpr ELitFalse st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (B False) st2 (cont ref)

evalExpr (EVar (Ident vid)) st cont = case M.lookup vid (vars st) of
  Nothing -> showError $ "Undefined variable: " ++ vid
  Just ref -> cont ref st

evalExpr (EList els) st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (L []) st2 c1 where
    c1 = foldr bindCont (cont ref) els where
      bindCont expr cont1 st4 = evalExpr expr st4 bindAppend where
        bindAppend ref2 st5 = listAppend ref ref2 st5 cont1

evalExpr (EListComp e lv eit) st cont = showError "Not implemented yet"

evalExpr (EDict ldm) st cont = showError "Not implemented yet"

truthHelper :: VRef -> PSt -> SCont -> SCont -> Result
truthHelper ref st tcont fcont = case store st M.! ref of
  B True  -> tcont st
  B False -> fcont st
  _       -> showError "Only Boolean values can be tested for truth"

toStrHelper :: Bool -> VRef -> PSt -> ECont -> Result
toStrHelper quoteStr ref st cont = case store st M.! ref of
  I i -> charList (show i) st cont
  C c -> charList ['\'', c, '\''] st cont
  L els -> if all (isCharRef st) els
              then if quoteStr
                then case unCharList st ref of
                  Nothing -> showError "Something went wrong!"
                  Just str -> charList ("\"" ++ str ++ "\"") st cont
                else cont ref st
              else alloc st c0 where
    c0 ref st2 = setStoreValue ref (L []) st2 c1 where
      c1 = foldr bindCont c2 els where
        c2 st6 = case unCharListList st6 ref of
          Nothing -> showError "Something went wrong!"
          Just str -> charList str st6 cont
        bindCont elref cont1 st4 = toStrHelper True elref st4 bindAppend where
          bindAppend ref2 st5 = listAppend ref ref2 st5 cont1
  B b -> charList (show b) st cont

charList :: String -> PSt -> ECont -> Result
charList str st cont = alloc st c0 where {
  c0 ref st2 = setStoreValue ref (L []) st2 c1 where {
    c1 = foldr bindCont (cont ref) str where {
      bindCont chr cont1 st4 = charVal chr st4 bindAppend where {
        bindAppend ref2 st5 = listAppend ref ref2 st5 cont1
}}}}

charVal :: Char -> PSt -> ECont -> Result
charVal c st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref (C c) st2 $ cont ref

listAppend :: VRef -> VRef -> PSt -> SCont -> Result
listAppend lref aref st cont = case store st M.! lref of
  L elems -> setStoreValue lref (L $ elems ++ [aref]) st cont
  _       -> showError "Not a list"

unCharListList :: PSt -> VRef -> Maybe String
unCharListList st ref = case store st M.! ref of
  L els -> do
    sl <- mapM (unCharList st) els
    return $ "[" ++ intercalate ", " sl ++ "]"
  _ -> Nothing

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

relOpFun :: (Eq a, Ord a) => RelOp -> (a -> a -> Bool)
relOpFun op = case op of
  Lt -> (<)
  Le -> (<=)
  Gt -> (>)
  Ge -> (>=)
  Eq -> (==)
  Ne -> (/=)

mulOpFun :: MulOp -> (Int -> Int -> Int)
mulOpFun op = case op of
  Mul -> (*)
  Div -> div
  Mod -> mod

idToStr :: Ident -> String
idToStr (Ident str) = str
