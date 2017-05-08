module StateModifiers where
import qualified Data.Map.Strict as M
import StateTypes

setStoreValue :: VRef -> Value -> SCont -> PSt -> Result
setStoreValue ref v cont st = cont (PSt (M.insert ref v $ store st) (nextRef st) (vars st) (input st))

setVarRef :: String -> VRef -> SCont -> PSt -> Result
setVarRef vname ref cont st = cont (PSt (store st) (nextRef st) (M.insert vname ref $ vars st) (input st))

alloc :: ECont -> PSt -> Result
alloc cont st = cont (nextRef st) st2 where
    st2 = PSt (store st) (incRef $ nextRef st) (vars st) (input st)

writeStdout :: String -> SCont -> PSt -> Result
writeStdout str cont st = (str ++ stdo, stde, waserr) where
    (stdo, stde, waserr) = cont st

writeStderr :: String -> SCont -> PSt -> Result
writeStderr str cont st = (stdo, str ++ stde, waserr) where
    (stdo, stde, waserr) = cont st

readChar :: ECont -> PSt -> Result
readChar cont st = if null (input st) then err else allocAndSet (C $ head (input st)) c0 st where
    err = showError "End of input"
    c0 ref st2 = cont ref $ PSt (store st2) (nextRef st2) (vars st2) (tail $ input st2)

showError :: String -> Result
showError str = ("", "Error: " ++ str ++ "\n", True)

allocAndSet :: Value -> ECont -> PSt -> Result
allocAndSet val cont = alloc c0 where
  c0 ref = setStoreValue ref val (cont ref)

ref2val :: PSt -> VRef -> Value
ref2val st ref = store st M.! ref

