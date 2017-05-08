module Builtins where
import qualified Data.Map.Strict as M
import StateTypes
import Expression
import StateModifiers

initialState :: String -> PSt
initialState = PSt initStore initRef initVars where
  initStore = M.fromList [
    (R (-1), strFun),
    (R (-2), readFun),
    (R (-3), oInit),
    (R (-4), oToStr),
    (R (-5), O (M.fromList [
      ("init", R (-3)),
      ("to_str", R(-4))]))]
  initVars = M.fromList [
    ("str", R (-1)),
    ("read", R (-2)),
    ("object", R(-5))]

strFun :: Value
strFun = F 1 c0 where
  c0 = toStrHelper True . head

readFun :: Value
readFun = F 0 c0 where
  c0 _ = readChar

oInit :: Value
oInit = F 1 c0 where
  c0 _ = allocAndSet (I 0)

oToStr :: Value
oToStr = F 1 c0 where
  c0 _ = charList "[object]"
