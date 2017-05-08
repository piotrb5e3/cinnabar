module Expression where
import Control.Monad (foldM)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import {-# SOURCE #-} Statement
import AbsCinnabar
import StateTypes
import StateModifiers
import Block

evalExpr :: Expr -> ECont -> PSt -> Result
evalExpr (ELambda ids e) cont st = allocAndSet (F (length ids) c0) cont st where
  c0 refs cont2 st2 = evalExpr e c1 st3 where
    st3 = PSt (store st2) (nextRef st2) newVars (input st2) where
      newVars = M.union (M.fromList (zip (map idToStr ids) refs)) $ vars st
    c1 ref st4 = cont2 ref $ PSt (store st4) (nextRef st4) (vars st2) (input st4)

evalExpr (EFun ids b) cont st = allocAndSet (F (length ids) c0) cont st where
  c0 refs cont2 st2 = runBlock b sc0 c1 st3 where
    st3 = PSt (store st2) (nextRef st2) newVars (input st2) where
      newVars = M.union (M.fromList (zip (map idToStr ids) refs)) $ vars st
    sc0 = allocAndSet (I 0) c1
    c1 ref st4 = cont2 ref $ PSt (store st4) (nextRef st4) (vars st2) (input st4)

evalExpr (EIf et ec ef) cont st = evalExpr ec c0 st where
  c0 ref = truthHelper ref tc fc where
    tc = evalExpr et cont
    fc = evalExpr ef cont

evalExpr (EOr e0 e1) cont st = evalExpr e0 c0 st where
  c0 ref = truthHelper ref (cont ref) c1 where
    c1 = evalExpr e1 c2 where
      c2 ref2 = truthHelper ref2 (cont ref2) (cont ref2) 

evalExpr (EAnd e0 e1) cont st = evalExpr e0 c0 st where
  c0 ref = truthHelper ref c1 (cont ref) where
    c1 = evalExpr e1 c2 where
      c2 ref2 = truthHelper ref2 (cont ref2) (cont ref2) 

evalExpr (ERel e0 op e1) cont st = eval2Expr e0 e1 c0 st where
  c0 ref ref2 st3 = case compareValues ref ref2 op st3 of
    Just b -> allocAndSet (B b) cont st3
    Nothing -> showError "Cannot compare"

evalExpr (EAdd e0 op e1) cont st = eval2Expr e0 e1 c0 st where
  c0 ref ref2 st2 = case (ref2val st2 ref, op, ref2val st2 ref2) of
    (I i1, Add, I i2) -> allocAndSet (I (i1 + i2)) cont st2
    (I i1, Sub, I i2) -> allocAndSet (I (i1 - i2)) cont st2
    (L refs, Add, L refs2 ) -> allocAndSet (L (refs ++ refs2)) cont st2
    (_, Add, _) -> showError "Cannot add"
    (_, Sub, _) -> showError "Cannot substract"

evalExpr (EMul e0 op e1) cont st = eval2Expr e0 e1 c0 st where
  c0 ref ref2 st2 = case (ref2val st2 ref, op, ref2val st2 ref2) of
    (I i1, Div, I 0) -> showError "Divide by 0 error"
    (I i1, Mod, I 0) -> showError "Divide by 0 error"
    (I i1, _, I i2) -> allocAndSet (I (mulOpFun op i1 i2)) cont st2
    (L refs, Mul, I i ) -> allocAndSet (L ([1..i] >> refs)) cont st2
    (_, Mul, _) -> showError "Cannot multiply"
    (_, Div, _) -> showError "Cannot divide"
    (_, Mod, _) -> showError "Cannot take modulo"

evalExpr (EPow e0 e1) cont st = eval2Expr e0 e1 c0 st where
  c0 ref ref2 st2 = case (ref2val st2 ref, ref2val st2 ref2) of
    (I i1, I i2) -> allocAndSet (I (i1 ^ i2)) cont st2
    _ -> showError "Cannot exponentiate"

evalExpr (ENot e) cont st = evalExpr e c0 st where
  c0 ref st2 = case ref2val st2 ref of
    B b -> allocAndSet (B (not b)) cont st2
    _   -> showError "Only boolean values can be negated"

evalExpr (ENeg e) cont st = evalExpr e c0 st where
  c0 ref st2 = case ref2val st2 ref of
    I i -> allocAndSet (I (-i)) cont st2
    _   -> showError "Only integer values can be inverted"

evalExpr (ECall e el) cont st = evalExpr e c0 st where
  c0 ref st2 = case ref2val st2 ref of
    F argc funC -> if length el /= argc
      then showError ("Wrong number of parameters. Expected: " ++ show argc ++ " was: " ++ show (length el))
      else evalExpr (EList el) c1 st2 where
        c1 ref2 st3 = case ref2val st3 ref2 of
          L rl -> funC rl cont st3
    _ -> showError "Called a non-callable value"

evalExpr (EMember e (Ident mid)) cont st = evalExpr e c0 st where
  c0 ref st2 = case ref2val st2 ref of
    L refs -> if mid == "length"
      then allocAndSet (I $ length refs) cont st2
      else showError ("List value has no member " ++ mid)
    D m -> case mid of
      "keys" -> allocAndSet (L $ M.keys m) cont st2
      "keys_values" -> allocAndSet (L []) c0 st2 where
        c0 ref = M.foldrWithKey procElem (cont ref) m where
          procElem k v cont1 = allocAndSet (L [k, v]) appendPair where
            appendPair pRef = listAppend ref pRef cont1
      _ -> showError ("Dictionary value has no member " ++ mid)
    O m -> case M.lookup mid m of
      Just ref2 -> case ref2val st2 ref2 of
        F i cont2 -> allocAndSet (F (i-1) (\lrefs -> cont2 (ref:lrefs))) cont st2
        _ -> cont ref2 st2
      Nothing -> showError ("Object has no member " ++ mid)
    _ -> showError ("Value has no member " ++ mid)

evalExpr (EAt e0 e1) cont st = eval2Expr e0 e1 c0 st where
  c0 ref ref2 st2 = case (ref2val st2 ref, ref2val st2 ref2) of
    (L l, I i) -> if i >= 0 && i < length l
      then cont (l !! i) st2
      else showError $ "List index out of range: " ++ show i
    (O m, L []) -> showError "Object member names must be non-empty strings"
    (O m, L lr) -> case unCharList st2 lr of
            Nothing -> showError "Object member names must be non-empty strings"
            Just str -> case M.lookup str m of
              Nothing -> showError ("Object has no member " ++ str)
              Just ref3 -> case ref2val st2 ref3 of
                F i cont2 -> allocAndSet (F (i-1) (\lrefs -> cont2 (ref:lrefs))) cont st2
                _ -> cont ref3 st2
    (O m, _) -> showError "Object member names must be non-empty strings"
    (D m, _) -> case dictKeyLookup m ref2 st2 of
      Just kRef -> cont (m M.! kRef) st2
      Nothing -> showError "Key does not exist in dict"
    (L _, _) -> showError "Bad list subscript type"
    _ -> showError "Type cannot be subscripted"

evalExpr (EExtend e0 e1) cont st = eval2Expr e0 e1 c0 st where
  c0 ref1 ref2 st2 = case (ref2val st2 ref1, ref2val st2 ref2) of
    (O m, D m2) -> case extendMerge m m2 st2 of
      Nothing -> showError "Extending with a dictionary containing a non-string key"
      Just m3 -> allocAndSet (O m3) cont st2
    (O m, _) -> showError "Extending with a non-dictionary value"
    _ -> showError "Extending a non-object value"

evalExpr (ENew e el) cont st = evalExpr e c0 st where
  c0 ref = evalExpr (EList el) c1 where
    c1 ref2 st3 = case ref2val st3 ref of
      O m -> case ref2val st3 $ m M.! "init" of
        F argc fCont -> if argc /= 1 + length el
          then showError ("Wrong number of parameters. Expected: " ++ show (argc - 1) ++ " was: " ++ show (length el))
          else case ref2val st3 ref2 of
            L refs -> allocAndSet (O m) c2 st3 where
              c2 nRef = fCont (nRef:refs) (const (cont nRef))
        _ -> showError "Object's member \"init\" in not a function"
      _ -> showError "Calling new with a non-object value"

evalExpr (EChar c) cont st = charVal c cont st

evalExpr (EString s) cont st = charList s cont st

evalExpr (ELitInt i) cont st = allocAndSet (I (fromIntegral i)) cont st

evalExpr ELitTrue cont st = allocAndSet (B True) cont st

evalExpr ELitFalse cont st = allocAndSet (B False) cont st

evalExpr (EVar (Ident vid)) cont st = case M.lookup vid (vars st) of
  Nothing -> showError $ "Undefined variable: " ++ vid
  Just ref -> cont ref st

evalExpr (EList els) cont st = allocAndSet (L []) c0 st where
    c0 ref = foldr bindCont (cont ref) els where
      bindCont expr cont1 = evalExpr expr bindAppend where
        bindAppend ref2 = listAppend ref ref2 cont1

evalExpr (EListComp e lv eit) cont st = evalExpr eit c0 st where
  c0 ref st2 = case ref2val st2 ref of
    L itRefs -> allocAndSet (L []) c1 st2 where
      c1 lRef st3 = foldr procElem cFin itRefs st3 where
        procElem ref2 cont2 = assignRefToLVal lv ref2 c2 where
          c2 = evalExpr e c3 where
            c3 ref3 =  listAppend lRef ref3 cont2
        cFin st7 = cont lRef finSt where
          finSt = PSt (store st7) (nextRef st7) (vars st3) (input st7)
    _ -> showError "Only lists can be iterated over in list comprehension"

evalExpr (EDict ldm) cont st = allocAndSet (D M.empty) c0 st where
  c0 dRef = foldr procElem (cont dRef) ldm where
    procElem (EDictMap e0 e1) cont2 = eval2Expr e0 e1 c1 where
      c1 ref1 ref2 = dictSet dRef ref1 ref2 cont2

eval2Expr :: Expr -> Expr -> DECont -> PSt -> Result
eval2Expr e0 e1 cont = evalExpr e0 c0 where
  c0 ref = evalExpr e1 $ cont ref

truthHelper :: VRef -> SCont -> SCont -> PSt -> Result
truthHelper ref tcont fcont st = case ref2val st ref of
  B True  -> tcont st
  B False -> fcont st
  _       -> showError "Only Boolean values can be tested for truth"

toStrHelper :: Bool -> VRef -> ECont -> PSt -> Result
toStrHelper quoteStr ref cont st = case ref2val st ref of
  I i -> charList (show i) cont st
  C c -> charList ['\'', c, '\''] cont st
  L [] -> charList "[]" cont st
  L els -> if all (isCharRef st) els
              then if quoteStr
                then case unCharList st els of
                  Just str -> charList ("\"" ++ str ++ "\"") cont st
                else cont ref st
              else allocAndSet (L []) c0 st where
      c0 ref = foldr bindCont c1 els where
        c1 st6 = case unCharListList st6 ref of
          Just str -> charList str cont st6
        bindCont elref cont1 = toStrHelper True elref bindAppend where
          bindAppend ref2 = listAppend ref ref2 cont1
  B b -> charList (show b) cont st
  D m -> allocAndSet (L []) c0 st where
    c0 ref = M.foldrWithKey procElem cFin m where
      cFin st6 = case unCharListDict st6 ref of
        Just str -> charList str cont st6
      procElem k v cont1 = toStrHelper True k procVal where
        procVal sKRef = toStrHelper True v mergeKV where
          mergeKV sVRef st4 = case (unCharListRef st4 sKRef, unCharListRef st4 sVRef) of
            (Just sk, Just sv) -> charList (sk ++ ": " ++ sv) appendKV st4 where
              appendKV sRef = listAppend ref sRef cont1
  F _ _ -> charList "<lambda>" cont st
  O m -> case ref2val st $ m M.! "to_str" of
    F 1 fCont -> fCont [ref] c0 st where
      c0 ref2 = toStrHelper False ref2 cont
    F i _ -> showError ("Object's to_str method has a wrong number of parameters. Expected: 1, found: " ++ show i)
    _ -> showError "Object's to_str member is not a function"

charList :: String -> ECont -> PSt -> Result
charList str cont = allocAndSet (L []) c0 where 
  c0 ref = foldr bindCont (cont ref) str where 
    bindCont chr cont1 = charVal chr bindAppend where 
      bindAppend ref2 = listAppend ref ref2 cont1


charVal :: Char -> ECont -> PSt -> Result
charVal c = allocAndSet (C c)

listAppend :: VRef -> VRef -> SCont -> PSt -> Result
listAppend lref aref cont st = case ref2val st lref of
  L elems -> setStoreValue lref (L $ elems ++ [aref]) cont st

dictKeyLookup :: M.Map VRef VRef -> VRef -> PSt -> Maybe VRef
dictKeyLookup m ref st = first compFun $ M.keys m where
  compFun ref2 = fromMaybe False $ compareValues ref ref2 Eq st

dictSet :: VRef -> VRef -> VRef -> SCont -> PSt -> Result
dictSet dRef ref0 ref1 cont st = case ref2val st dRef of
  D m -> setStoreValue dRef (D $ M.insert kRef ref1 m) cont st where
    kRef = fromMaybe ref0 $ dictKeyLookup m ref0 st

objectSet :: VRef -> String -> VRef -> SCont -> PSt -> Result
objectSet oRef mid ref1 cont st = case ref2val st oRef of
  O m -> setStoreValue oRef (O $ M.insert mid ref1 m) cont st

extendMerge :: M.Map String VRef -> M.Map VRef VRef -> PSt -> Maybe (M.Map String VRef)
extendMerge m m1 st = do
   let (kl, vl) = unzip $ M.toList m1
   kl<- mapM (unCharListRef st) kl
   let m1' = M.fromList $ zip kl vl
   return $ M.union m1' m
  

unCharListList :: PSt -> VRef -> Maybe String
unCharListList st ref = case store st M.! ref of
  L els -> do
    sl <- mapM (unCharListRef st) els
    return $ "[" ++ intercalate ", " sl ++ "]"
  _ -> Nothing

unCharListDict :: PSt -> VRef -> Maybe String
unCharListDict st ref = case store st M.! ref of
  L els -> do
    sl <- mapM (unCharListRef st) els
    return $ "#{" ++ intercalate ", " sl ++ "}"
  _ -> Nothing

unCharList :: PSt -> [VRef] -> Maybe String
unCharList st = mapM (unValChar st)

unCharListRef ::  PSt -> VRef -> Maybe String
unCharListRef st ref = case ref2val st ref of
  L els -> unCharList st els
  _ -> Nothing

unValChar :: PSt -> VRef -> Maybe Char
unValChar st ref = case ref2val st ref of
  C c -> Just c
  _   -> Nothing

isCharRef :: PSt -> VRef -> Bool
isCharRef st ref = case ref2val st ref of
  C _ -> True
  _   -> False

compareValues :: VRef -> VRef -> RelOp -> PSt -> Maybe Bool
compareValues ref1 ref2 op st = case (ref2val st ref1, op, ref2val st ref2) of
    (I i1, _, I i2) -> return $ relOpFun op i1 i2
    (C c1, _, C c2) -> return $ relOpFun op c1 c2
    (B b1, Eq, B b2) -> return $ b1 == b2
    (B b1, Ne, B b2) -> return $ b1 /= b2
    (L l1, Eq, L l2) -> listEqCompare l1 l2 st
    (L l1, Ne, L l2) -> do
      res <- listEqCompare l1 l2 st
      return $ not res
    (D d1, Eq, D d2) -> dictEqCompare d1 d2 st
    (D d1, Ne, D d2) -> do
      res <- dictEqCompare d1 d2 st
      return $ not res
    (O _, Eq, O _) -> return $ ref1 == ref2
    (O _, Ne, O _) -> return $ ref1 /= ref2
    (F _ _, Eq, F _ _) -> return $ ref1 == ref2
    (F _ _, Ne, F _ _) -> return $ ref1 /= ref2
    _ -> Nothing

listEqCompare :: [VRef] -> [VRef] -> PSt -> Maybe Bool
listEqCompare l1 l2 st = if length l1 /= length l2
  then return False
  else do
    compRes <- mapM (\(r1, r2) -> compareValues r1 r2 Eq st) $ zip l1 l2
    return $ and compRes

dictEqCompare :: M.Map VRef VRef -> M.Map VRef VRef -> PSt -> Maybe Bool
dictEqCompare m1 m2 st = if M.size m1 /= M.size m2
  then return False
  else do
    let k1 = M.keys m1
    case mapM (\r -> dictKeyLookup m2 r st) k1 of
      Nothing -> return False
      Just k2 -> do
        compRes <- mapM (\(r1, r2) -> compareValues (m1 M.! r1) (m2 M.! r2) Eq st) $ zip k1 k2
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

