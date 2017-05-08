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
evalExpr (ELambda ids e) cont = c0 where
  c0 st = allocAndSet (F (length ids) c1) cont st where
    c1 refs cont2 st2 = evalExpr e c2 st3 where
      st3 = PSt (store st2) (nextRef st2) newVars (input st2) where
        newVars = M.union (M.fromList (zip (map idToStr ids) refs)) $ vars st
      c2 ref st4 = cont2 ref $ PSt (store st4) (nextRef st4) (vars st2) (input st4)

evalExpr (EFun ids b) cont = c0 where
  c0 st = allocAndSet (F (length ids) c1) cont st where
    c1 refs cont2 st2 = runBlock b sc0 c2 st3 where
      st3 = PSt (store st2) (nextRef st2) newVars (input st2) where
        newVars = M.union (M.fromList (zip (map idToStr ids) refs)) $ vars st
      sc0 = allocAndSet (I 0) c2
      c2 ref st4 = cont2 ref $ PSt (store st4) (nextRef st4) (vars st2) (input st4)

evalExpr (EIf et ec ef) cont = evalExpr ec c0 where
  c0 ref = truthHelper ref tc fc
  tc = evalExpr et cont
  fc = evalExpr ef cont

evalExpr (EOr e0 e1) cont = evalExpr e0 c0 where
  c0 ref = truthHelper ref (cont ref) c1
  c1 = evalExpr e1 c2
  c2 ref = truthHelper ref (cont ref) (cont ref) 

evalExpr (EAnd e0 e1) cont = evalExpr e0 c0 where
  c0 ref = truthHelper ref c1 (cont ref)
  c1 = evalExpr e1 c2 
  c2 ref = truthHelper ref (cont ref) (cont ref) 

evalExpr (ERel e0 op e1) cont = eval2Expr e0 e1 c0 where
  c0 ref ref2 st3 = case compareValues ref ref2 op st3 of
    Just b -> allocAndSet (B b) cont st3
    Nothing -> showError "Cannot compare"

evalExpr (EAdd e0 op e1) cont = eval2Expr e0 e1 c0 where
  c0 ref ref2 st = case (ref2val st ref, op, ref2val st ref2) of
    (I i1, Add, I i2) -> allocAndSet (I (i1 + i2)) cont st
    (I i1, Sub, I i2) -> allocAndSet (I (i1 - i2)) cont st
    (L refs, Add, L refs2 ) -> allocAndSet (L (refs ++ refs2)) cont st
    (_, Add, _) -> showError "Cannot add"
    (_, Sub, _) -> showError "Cannot substract"

evalExpr (EMul e0 op e1) cont = eval2Expr e0 e1 c0 where
  c0 ref ref2 st = case (ref2val st ref, op, ref2val st ref2) of
    (I i1, Div, I 0) -> showError "Divide by 0 error"
    (I i1, Mod, I 0) -> showError "Divide by 0 error"
    (I i1, _, I i2) -> allocAndSet (I (mulOpFun op i1 i2)) cont st
    (L refs, Mul, I i ) -> allocAndSet (L ([1..i] >> refs)) cont st
    (_, Mul, _) -> showError "Cannot multiply"
    (_, Div, _) -> showError "Cannot divide"
    (_, Mod, _) -> showError "Cannot take modulo"

evalExpr (EPow e0 e1) cont = eval2Expr e0 e1 c0 where
  c0 ref ref2 st = case (ref2val st ref, ref2val st ref2) of
    (I i1, I i2) -> allocAndSet (I (i1 ^ i2)) cont st
    _ -> showError "Cannot exponentiate"

evalExpr (ENot e) cont = evalExpr e c0 where
  c0 ref st = case ref2val st ref of
    B b -> allocAndSet (B (not b)) cont st
    _   -> showError "Only boolean values can be negated"

evalExpr (ENeg e) cont = evalExpr e c0 where
  c0 ref st = case ref2val st ref of
    I i -> allocAndSet (I (-i)) cont st
    _   -> showError "Only integer values can be inverted"

evalExpr (ECall e el) cont = evalExpr e c0 where
  c0 ref st = case ref2val st ref of
    F argc funC -> if length el /= argc
      then showError ("Wrong number of parameters. Expected: " ++ show argc ++ " was: " ++ show (length el))
      else evalExpr (EList el) c1 st where
        c1 ref2 st2 = case ref2val st2 ref2 of
          L rl -> funC rl cont st2
    _ -> showError "Called a non-callable value"

evalExpr (EMember e (Ident mid)) cont = evalExpr e c0 where
  c0 ref st = case ref2val st ref of
    L refs -> case mid of
      "length" -> allocAndSet (I $ length refs) cont st
      _ -> showError ("List value has no member " ++ mid)
    D m -> case mid of
      "keys" -> allocAndSet (L $ M.keys m) cont st
      "keys_values" -> allocAndSet (L []) c0 st where
        c0 ref = M.foldrWithKey procElem (cont ref) m where
          procElem k v cont1 = allocAndSet (L [k, v]) appendPair where
            appendPair pRef = listAppend ref pRef cont1
      _ -> showError ("Dictionary value has no member " ++ mid)
    O m -> case M.lookup mid m of
      Just ref2 -> case ref2val st ref2 of
        F i fCont -> allocAndSet (F (i - 1) fCont2) cont st where
          fCont2 lrefs = fCont (ref:lrefs)
        _ -> cont ref2 st
      Nothing -> showError ("Object has no member " ++ mid)
    _ -> showError ("Value has no member " ++ mid)

evalExpr (EAt e0 e1) cont = eval2Expr e0 e1 c0 where
  c0 ref ref2 st = case (ref2val st ref, ref2val st ref2) of
    (L l, I i) -> if i >= 0 && i < length l
      then cont (l !! i) st
      else showError $ "List index out of range: " ++ show i
    (L _, _) -> showError "Bad list subscript type"
    (O m, L []) -> showError "Object member names must be non-empty strings"
    (O m, L lr) -> case unCharList st lr of
            Nothing -> showError "Object member names must be non-empty strings"
            Just str -> case M.lookup str m of
              Nothing -> showError ("Object has no member " ++ str)
              Just ref3 -> case ref2val st ref3 of
                F i fCont -> allocAndSet (F (i - 1) fCont2) cont st where
                  fCont2 lrefs = fCont (ref:lrefs)
                _ -> cont ref3 st
    (O m, _) -> showError "Object member names must be non-empty strings"
    (D m, _) -> case dictKeyLookup m ref2 st of
      Just kRef -> cont (m M.! kRef) st
      Nothing -> showError "Key does not exist in dict"
    _ -> showError "Type cannot be subscripted"

evalExpr (EExtend e0 e1) cont = eval2Expr e0 e1 c0 where
  c0 ref ref2 st = case (ref2val st ref, ref2val st ref2) of
    (O m, D m2) -> case extendMerge m m2 st of
      Nothing -> showError "Extending with a dictionary containing a non-string key"
      Just m3 -> allocAndSet (O m3) cont st
    (O m, _) -> showError "Extending with a non-dictionary value"
    _ -> showError "Extending a non-object value"

evalExpr (ENew e) cont = evalExpr e c0 where
  c0 ref st = case ref2val st ref of
    O m -> case ref2val st $ m M.! "init" of
      F argc fCont -> allocAndSet (F (argc - 1) fCont2) cont st where
        fCont2 refs cont2 = allocAndSet (O m) c1 where
         c1 nRef = fCont (nRef:refs) (const (cont2 nRef))
      _ -> showError "Object's member \"init\" in not a function"
    _ -> showError "Calling new with a non-object value"

evalExpr (EChar c) cont = charVal c cont
evalExpr (EString s) cont = charList s cont
evalExpr (ELitInt i) cont = allocAndSet (I (fromIntegral i)) cont
evalExpr ELitTrue cont = allocAndSet (B True) cont
evalExpr ELitFalse cont = allocAndSet (B False) cont

evalExpr (EVar (Ident vid)) cont = \st -> case M.lookup vid (vars st) of
  Nothing -> showError $ "Undefined variable: " ++ vid
  Just ref -> cont ref st

evalExpr (EList els) cont = allocAndSet (L []) c0 where
    c0 ref = foldr procElem (cont ref) els where
      procElem expr cont1 = evalExpr expr appendElem where
        appendElem ref2 = listAppend ref ref2 cont1

evalExpr (EListComp e lv eit) cont = evalExpr eit c0 where
  c0 ref st = case ref2val st ref of
    L itRefs -> allocAndSet (L []) c1 st where
      c1 lRef = foldr procElem cFin itRefs where
        cFin st2 = cont lRef $ PSt (store st2) (nextRef st2) (vars st) (input st2)
        procElem ref2 cont2 = assignRefToLVal lv ref2 (evalExpr e c2) where
          c2 ref3 =  listAppend lRef ref3 cont2
    _ -> showError "Only lists can be iterated over in list comprehension"

evalExpr (EDict dms) cont = allocAndSet (D M.empty) c0 where
  c0 dRef = foldr procElem (cont dRef) dms where
    procElem (EDictMap e0 e1) cont2 = eval2Expr e0 e1 c1 where
      c1 ref1 ref2 = dictSet dRef ref1 ref2 cont2

eval2Expr :: Expr -> Expr -> DECont -> PSt -> Result
eval2Expr e0 e1 cont = evalExpr e0 (evalExpr e1 . cont)

truthHelper :: VRef -> SCont -> SCont -> PSt -> Result
truthHelper ref tcont fcont st = case ref2val st ref of
  B True  -> tcont st
  B False -> fcont st
  _       -> showError "Only Boolean values can be tested for truth"

toStrHelper :: Bool -> VRef -> ECont -> PSt -> Result
toStrHelper quoteStr ref cont st = case ref2val st ref of
  I i -> charList (show i) cont st
  C c -> charList ['\'', c, '\''] cont st
  L [] -> if quoteStr
    then charList "[]" cont st
    else cont ref st
  L els -> if all (isCharRef st) els
    then if quoteStr
      then case unCharList st els of
        Just str -> charList ("\"" ++ str ++ "\"") cont st
      else cont ref st
    else allocAndSet (L []) c0 st where
      c0 ref = foldr procElem cFin els where
        cFin st2 = case unCharListList st2 ref of
          Just str -> charList str cont st2
        procElem elRef cont1 = toStrHelper True elRef appendElem where
          appendElem ref2 = listAppend ref ref2 cont1
  B b -> charList (show b) cont st
  D m -> allocAndSet (L []) c0 st where
    c0 ref = M.foldrWithKey procElem cFin m where
      cFin st2 = case unCharListDict st2 ref of
        Just str -> charList str cont st2
      procElem k v cont1 = toStrHelper True k procVal where
        procVal sKRef = toStrHelper True v mergeKV where
          mergeKV sVRef st3 = case (unCharListRef st3 sKRef, unCharListRef st3 sVRef) of
            (Just sk, Just sv) -> charList (sk ++ ": " ++ sv) appendKV st3 where
              appendKV sRef = listAppend ref sRef cont1
  F _ _ -> charList "<function>" cont st
  O m -> case ref2val st $ m M.! "to_str" of
    F 1 fCont -> fCont [ref] c0 st where
      c0 ref2 = toStrHelper False ref2 cont
    F i _ -> showError ("Object's to_str method has a wrong number of parameters. Expected: 1, found: " ++ show i)
    _ -> showError "Object's to_str member is not a function"

charList :: String -> ECont -> PSt -> Result
charList str cont = allocAndSet (L []) c0 where 
  c0 ref = foldr procChar (cont ref) str where 
    procChar chr cont1 = charVal chr appendChar where 
      appendChar ref2 = listAppend ref ref2 cont1

charVal :: Char -> ECont -> PSt -> Result
charVal c = allocAndSet (C c)

listAppend :: VRef -> VRef -> SCont -> PSt -> Result
listAppend lRef eRef cont st = case ref2val st lRef of
  L elems -> setStoreValue lRef (L $ elems ++ [eRef]) cont st

dictKeyLookup :: M.Map VRef VRef -> VRef -> PSt -> Maybe VRef
dictKeyLookup m ref st = first compFun $ M.keys m where
  compFun ref2 = fromMaybe False $ compareValues ref ref2 Eq st

dictSet :: VRef -> VRef -> VRef -> SCont -> PSt -> Result
dictSet dRef kRef vRef cont st = case ref2val st dRef of
  D m -> setStoreValue dRef (D $ M.insert kRef' vRef m) cont st where
    kRef' = fromMaybe kRef $ dictKeyLookup m kRef st

objectSet :: VRef -> String -> VRef -> SCont -> PSt -> Result
objectSet oRef mid vRef cont st = case ref2val st oRef of
  O m -> setStoreValue oRef (O $ M.insert mid vRef m) cont st

extendMerge :: M.Map String VRef -> M.Map VRef VRef -> PSt -> Maybe (M.Map String VRef)
extendMerge m m1 st = do
   let (kl, vl) = unzip $ M.toList m1
   kl <- mapM (unCharListRef st) kl
   let m1' = M.fromList $ zip kl vl
   return $ M.union m1' m

unCharListList :: PSt -> VRef -> Maybe String
unCharListList st ref = case ref2val st ref of
  L els -> do
    sl <- mapM (unCharListRef st) els
    return $ "[" ++ intercalate ", " sl ++ "]"
  _ -> Nothing

unCharListDict :: PSt -> VRef -> Maybe String
unCharListDict st ref = case ref2val st ref of
  L els -> do
    sl <- mapM (unCharListRef st) els
    return $ "#{" ++ intercalate ", " sl ++ "}"
  _ -> Nothing

unCharList :: PSt -> [VRef] -> Maybe String
unCharList = mapM . unValChar

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
    (L l1, Ne, L l2) -> not <$> listEqCompare l1 l2 st
    (D d1, Eq, D d2) -> dictEqCompare d1 d2 st
    (D d1, Ne, D d2) -> not <$> dictEqCompare d1 d2 st
    (O _, Eq, O _) -> return $ ref1 == ref2
    (O _, Ne, O _) -> return $ ref1 /= ref2
    (F _ _, Eq, F _ _) -> return $ ref1 == ref2
    (F _ _, Ne, F _ _) -> return $ ref1 /= ref2
    _ -> Nothing

listEqCompare :: [VRef] -> [VRef] -> PSt -> Maybe Bool
listEqCompare l1 l2 st = if length l1 /= length l2
  then return False
  else fmap and $ mapM (\(r1, r2) -> compareValues r1 r2 Eq st) $ zip l1 l2

dictEqCompare :: M.Map VRef VRef -> M.Map VRef VRef -> PSt -> Maybe Bool
dictEqCompare m1 m2 st = if M.size m1 /= M.size m2
  then return False
  else do
    let k1 = M.keys m1
    case mapM (\r -> dictKeyLookup m2 r st) k1 of
      Nothing -> return False
      Just k2 -> fmap and $ mapM (\(r1, r2) -> compareValues (m1 M.! r1) (m2 M.! r2) Eq st) $ zip k1 k2

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

