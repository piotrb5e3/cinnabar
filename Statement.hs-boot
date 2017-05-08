module Statement where
import AbsCinnabar
import StateTypes

runStatement :: Stmt -> SCont -> ECont -> PSt -> Result
assignRefToLVal :: LVal -> VRef -> SCont -> PSt -> Result

