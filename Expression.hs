module Expression where
import Control.Monad (foldM)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import {-# SOURCE #-} Statement
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

evalExpr (EIf et ec ef) st cont = evalExpr ec st c0 where
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

evalExpr (ERel e0 op e1) st cont = eval2Expr e0 e1 st c0 where
  c0 ref ref2 st3 = case compareValues ref ref2 st3 op of
    Just b -> allocAndSet (B b) st3 cont
    Nothing -> showError "Cannot compare"

evalExpr (EAdd e0 op e1) st cont = eval2Expr e0 e1 st c0 where
  c0 ref ref2 st2 = case (ref2val st2 ref, op, ref2val st2 ref2) of
    (I i1, Add, I i2) -> allocAndSet (I (i1 + i2)) st2 cont
    (I i1, Sub, I i2) -> allocAndSet (I (i1 - i2)) st2 cont
    (L refs, Add, L refs2 ) -> allocAndSet (L (refs ++ refs2)) st2 cont
    (_, Add, _) -> showError "Cannot add"
    (_, Sub, _) -> showError "Cannot substract"

evalExpr (EMul e0 op e1) st cont = eval2Expr e0 e1 st c0 where
  c0 ref ref2 st2 = case (ref2val st2 ref, op, ref2val st2 ref2) of
    (I i1, Div, I 0) -> showError "Divide by 0 error"
    (I i1, Mod, I 0) -> showError "Divide by 0 error"
    (I i1, _, I i2) -> allocAndSet (I (mulOpFun op i1 i2)) st2 cont
    (L refs, Mul, I i ) -> allocAndSet (L ([1..i] >> refs)) st2 cont
    (_, Mul, _) -> showError "Cannot multiply"
    (_, Div, _) -> showError "Cannot divide"
    (_, Mod, _) -> showError "Cannot take modulo"

evalExpr (EPow e0 e1) st cont = eval2Expr e0 e1 st c0 where
  c0 ref ref2 st2 = case (ref2val st2 ref, ref2val st2 ref2) of
    (I i1, I i2) -> allocAndSet (I (i1 ^ i2)) st2 cont
    _ -> showError "Cannot exponentiate"

evalExpr (ENot e) st cont = evalExpr e st c0 where
  c0 ref st2 = case ref2val st2 ref of
    B b -> allocAndSet (B (not b)) st2 cont
    _   -> showError "Only boolean values can be negated"

evalExpr (ENeg e) st cont = evalExpr e st c0 where
  c0 ref st2 = case ref2val st2 ref of
    I i -> allocAndSet (I (-i)) st2 cont
    _   -> showError "Only integer values can be inverted"

evalExpr (ECall e el) st cont = evalExpr e st c0 where
  c0 ref st2 = case ref2val st2 ref of
    F argc funC -> if length el /= argc
      then showError ("Wrong number of parameters. Expected: " ++ show argc ++ " was: " ++ show (length el))
      else evalExpr (EList el) st2 c1 where
        c1 ref2 st3 = case ref2val st3 ref2 of
          L rl -> funC rl st3 cont
          _ -> showError "Internal error (ECall)"
    _ -> showError "Called a non-callable value"

evalExpr (EMember e mid) st cont = showError "Not implemented yet"

evalExpr (EAt e0 e1) st cont = eval2Expr e0 e1 st c0 where
  c0 ref ref2 st2 = case (ref2val st2 ref, ref2val st2 ref2) of
    (L l, I i) -> if i >= 0 && i < length l
      then cont (l !! i) st2
      else showError $ "List index out of range: " ++ show i
    (O m, _) -> showError "Not implemented yet"
    (D m, _) -> case dictKeyLookup m ref2 st2 of
      Just kRef -> cont (m M.! kRef) st2
      Nothing -> showError "Key does not exist in dict"
    (L _, _) -> showError "Bad list subscript type"
    _ -> showError "Type cannot be subscripted"

evalExpr (EExtend e0 e1) st cont = showError "Not implemented yet"

evalExpr (ENew e el) st cont = showError "Not implemented yet"

evalExpr (EChar c) st cont = charVal c st cont

evalExpr (EString s) st cont = charList s st cont

evalExpr (ELitInt i) st cont = allocAndSet (I (fromIntegral i)) st cont

evalExpr ELitTrue st cont = allocAndSet (B True) st cont

evalExpr ELitFalse st cont = allocAndSet (B False) st cont

evalExpr (EVar (Ident vid)) st cont = case M.lookup vid (vars st) of
  Nothing -> showError $ "Undefined variable: " ++ vid
  Just ref -> cont ref st

evalExpr (EList els) st cont = allocAndSet (L []) st c0 where
    c0 ref = foldr bindCont (cont ref) els where
      bindCont expr cont1 st4 = evalExpr expr st4 bindAppend where
        bindAppend ref2 st5 = listAppend ref ref2 st5 cont1

evalExpr (EListComp e lv eit) st cont = evalExpr eit st c0 where
  c0 ref st2 = case ref2val st2 ref of
    L itRefs -> allocAndSet (L []) st2 c1 where
      c1 lRef st3 = foldr procElem cFin itRefs st3 where
        procElem ref2 cont2 st4 = assignRefToLVal lv ref2 st4 c2 where
          c2 st5 = evalExpr e st5 c3 where
            c3 ref3 st6 =  listAppend lRef ref3 st6 cont2
        cFin st7 = cont lRef finSt where
          finSt = PSt (store st7) (nextRef st7) (vars st3) (input st7)
    _ -> showError "Only lists can be iterated over in list comprehension"

evalExpr (EDict ldm) st cont = allocAndSet (D M.empty) st c0 where
  c0 dRef = foldr procElem (cont dRef) ldm where
    procElem (EDictMap e0 e1) cont2 st3 = eval2Expr e0 e1 st3 c1 where
      c1 ref1 ref2 st4 = dictSet dRef ref1 ref2 st4 cont2

eval2Expr :: Expr -> Expr -> PSt -> DECont -> Result
eval2Expr e0 e1 st cont = evalExpr e0 st c0 where
  c0 ref st2 = evalExpr e1 st2 $ cont ref

truthHelper :: VRef -> PSt -> SCont -> SCont -> Result
truthHelper ref st tcont fcont = case ref2val st ref of
  B True  -> tcont st
  B False -> fcont st
  _       -> showError "Only Boolean values can be tested for truth"

toStrHelper :: Bool -> VRef -> PSt -> ECont -> Result
toStrHelper quoteStr ref st cont = case ref2val st ref of
  I i -> charList (show i) st cont
  C c -> charList ['\'', c, '\''] st cont
  L [] -> charList "[]" st cont
  L els -> if all (isCharRef st) els
              then if quoteStr
                then case unCharList st ref of
                  Nothing -> showError "Internal error (toStrHelper)"
                  Just str -> charList ("\"" ++ str ++ "\"") st cont
                else cont ref st
              else allocAndSet (L []) st c0 where
      c0 ref = foldr bindCont c1 els where
        c1 st6 = case unCharListList st6 ref of
          Nothing -> showError "Internal error (toStrHelper 2)"
          Just str -> charList str st6 cont
        bindCont elref cont1 st4 = toStrHelper True elref st4 bindAppend where
          bindAppend ref2 st5 = listAppend ref ref2 st5 cont1
  B b -> charList (show b) st cont
  D m -> allocAndSet (L []) st c0 where
    c0 ref = M.foldrWithKey procElem cFin m where
      cFin st6 = case unCharListDict st6 ref of
        Just str -> charList str st6 cont
      procElem k v cont1 st2 = toStrHelper True k st2 procVal where
        procVal sKRef st3 = toStrHelper True v st3 mergeKV where
          mergeKV sVRef st4 = case (unCharList st4 sKRef, unCharList st4 sVRef) of
            (Just sk, Just sv) -> charList (sk ++ ": " ++ sv) st4 appendKV where
              appendKV sRef st5 = listAppend ref sRef st5 cont1
  F _ _ -> charList "<lambda>" st cont
  _ -> showError "Not implemented yet"

charList :: String -> PSt -> ECont -> Result
charList str st cont = allocAndSet (L []) st c0 where 
  c0 ref = foldr bindCont (cont ref) str where 
    bindCont chr cont1 st4 = charVal chr st4 bindAppend where 
      bindAppend ref2 st5 = listAppend ref ref2 st5 cont1


charVal :: Char -> PSt -> ECont -> Result
charVal c = allocAndSet (C c)

listAppend :: VRef -> VRef -> PSt -> SCont -> Result
listAppend lref aref st cont = case ref2val st lref of
  L elems -> setStoreValue lref (L $ elems ++ [aref]) st cont
  _       -> showError "Internal error (listAppend)"

dictKeyLookup :: M.Map VRef VRef -> VRef -> PSt -> Maybe VRef
dictKeyLookup m ref st = first compFun $ M.keys m where
  compFun ref2 = fromMaybe False $ compareValues ref ref2 st Eq

dictSet :: VRef -> VRef -> VRef -> PSt -> SCont -> Result
dictSet dRef ref0 ref1 st cont = case ref2val st dRef of
  D m -> setStoreValue dRef (D $ M.insert kRef ref1 m) st cont where
    kRef = fromMaybe ref0 $ dictKeyLookup m ref0 st
  _ -> showError "Internal error (dictSet)"

unCharListList :: PSt -> VRef -> Maybe String
unCharListList st ref = case store st M.! ref of
  L els -> do
    sl <- mapM (unCharList st) els
    return $ "[" ++ intercalate ", " sl ++ "]"
  _ -> Nothing

unCharListDict :: PSt -> VRef -> Maybe String
unCharListDict st ref = case store st M.! ref of
  L els -> do
    sl <- mapM (unCharList st) els
    return $ "#{" ++ intercalate ", " sl ++ "}"
  _ -> Nothing

unCharList ::  PSt -> VRef -> Maybe String
unCharList st ref = case ref2val st ref of
  L els -> mapM (unValChar st) els
  _ -> Nothing

unValChar :: PSt -> VRef -> Maybe Char
unValChar st ref = case ref2val st ref of
  C c -> Just c
  _   -> Nothing

isCharRef :: PSt -> VRef -> Bool
isCharRef st ref = case ref2val st ref of
  C _ -> True
  _   -> False

compareValues :: VRef -> VRef -> PSt -> RelOp -> Maybe Bool
compareValues ref1 ref2 st op = case (ref2val st ref1, op, ref2val st ref2) of
    (I i1, _, I i2) -> return $ relOpFun op i1 i2
    (C c1, _, C c2) -> return $ relOpFun op c1 c2
    (B b1, Eq, B b2) -> return $ b1 == b2
    (B b1, Ne, B b2) -> return $ b1 /= b2
    (L l1, Eq, L l2) -> listEqCompare l1 l2 st
    (L l1, Ne, L l2) -> do
      res <- listEqCompare l1 l2 st
      return $ not res
    (D d1, _, D d2) -> Nothing
    (O o1, _, O o2) -> Nothing
    _ -> Nothing

listEqCompare :: [VRef] -> [VRef] -> PSt -> Maybe Bool
listEqCompare l1 l2 st = if length l1 /= length l2
  then return False
  else do
    compRes <- mapM (\(l1, l2) -> compareValues l1 l2 st Eq) $ zip l1 l2
    return $ and compRes

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

first :: (a -> Bool) -> [a] -> Maybe a
first f [] = Nothing
first f (l:ls) = if f l then Just l else first f ls

