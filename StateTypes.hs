module StateTypes where
import qualified Data.Map.Strict as M

type Stdout = String
type Stderr = String
type WasErr = Bool
type Result = (Stdout, Stderr, WasErr)

newtype VRef = R Int deriving (Eq, Ord)

incRef :: VRef -> VRef
incRef (R i) = R $ i + 1
initRef = R 0

data Value = I Int | C Char | L [VRef] | B Bool | D (M.Map VRef VRef) | O (M.Map String VRef) | F Int ([VRef] -> ECont -> PSt -> Result)

type SCont = PSt -> Result
type ECont = VRef -> PSt -> Result
type DECont = VRef -> VRef -> PSt -> Result

data PSt = PSt { store :: M.Map VRef Value
               , nextRef :: VRef
               , vars  :: M.Map String VRef
               , input :: String
               }


