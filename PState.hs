module PState where
import qualified Data.Map.Strict as M

import {-# SOURCE #-} Expression

type Stdout = String
type Stderr = String
type WasErr = Bool
type Result = (Stdout, Stderr, WasErr)

newtype VRef = R Int deriving (Eq, Ord)

incRef :: VRef -> VRef
incRef (R i) = R $ i + 1
initRef = R 0

data Value = I Int | C Char | L [VRef] | B Bool | D (M.Map VRef VRef) | O (M.Map String VRef) | F Int ([VRef] -> PSt -> ECont -> Result)

type SCont = PSt -> Result
type ECont = VRef -> PSt -> Result
type DECont = VRef -> VRef -> PSt -> Result

data PSt = PSt { store :: M.Map VRef Value
               , nextRef :: VRef
               , vars  :: M.Map String VRef
               , input :: String
               }

initialState :: String -> PSt
initialState = PSt initStore  initRef initVars where
  initStore = M.fromList [(R (-1), strFun), (R (-2), readFun)]
  initVars = M.fromList [("str", R (-1)), ("read", R (-2))]

setStoreValue :: VRef -> Value -> PSt -> SCont -> Result
setStoreValue ref v st cont = cont (PSt (M.insert ref v $ store st) (nextRef st) (vars st) (input st))

setVarRef :: String -> VRef -> PSt -> SCont -> Result
setVarRef vname ref st cont = cont (PSt (store st) (nextRef st) (M.insert vname ref $ vars st) (input st))

alloc :: PSt -> ECont -> Result
alloc st cont = cont (nextRef st) st2 where
    st2 = PSt (store st) (incRef $ nextRef st) (vars st) (input st)

writeStdout :: String -> PSt -> SCont -> Result
writeStdout str st cont = (str ++ stdo, stde, waserr) where
    (stdo, stde, waserr) = cont st

writeStderr :: String -> PSt -> SCont -> Result
writeStderr str st cont = (stdo, str ++ stde, waserr) where
    (stdo, stde, waserr) = cont st

readChar :: PSt -> ECont -> Result
readChar st cont = if null (input st) then err else alloc st c0 where
    err = showError "End of input"
    c0 ref st2 = setStoreValue ref (C $ head (input st2)) st2 c1 where
      c1 st3 = cont ref $ PSt (store st3) (nextRef st3) (vars st3) (tail $ input st3)

showError :: String -> Result
showError str = ("", "Error: " ++ str ++ "\n", True)

allocAndSet :: Value -> PSt -> ECont -> Result
allocAndSet val st cont = alloc st c0 where
  c0 ref st2 = setStoreValue ref val st2 (cont ref)

ref2val :: PSt -> VRef -> Value
ref2val st ref = store st M.! ref

---- Builtins ----

strFun :: Value
strFun = F 1 c0 where
  c0 = toStrHelper True . head

readFun :: Value
readFun = F 0 c0 where
  c0 _ = readChar
