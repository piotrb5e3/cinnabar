module Values where
import Data.Map as M

newtype VRef = R Integer deriving (Eq, Ord)

incRef :: VRef -> VRef
incRef (R i) = R $ i + 1
initRef = R 0

data Value = I Integer | C Char | L [VRef] | B Bool | D (M.Map VRef VRef) | O (M.Map String VRef) | F Integer
