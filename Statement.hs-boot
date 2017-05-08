module Statement where
import AbsCinnabar
import StateTypes

runStatement :: Stmt -> PSt -> SCont -> ECont -> Result
assignRefToLVal :: LVal -> VRef -> PSt -> SCont -> Result

