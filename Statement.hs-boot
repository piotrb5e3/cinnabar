module Statement where
import AbsCinnabar
import {-# SOURCE #-} PState

runStatement :: Stmt -> PSt -> SCont -> ECont -> Result

