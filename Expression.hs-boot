module Expression where
import Data.List (intercalate)
import qualified Data.Map.Strict as M

import AbsCinnabar
import {-# SOURCE #-} PState
import Block

evalExpr :: Expr -> PSt -> ECont -> Result
truthHelper :: VRef -> PSt -> SCont -> SCont -> Result
toStrHelper :: Bool -> VRef -> PSt -> ECont -> Result
charList :: String -> PSt -> ECont -> Result

