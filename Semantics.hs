module Language.Semantics where

import Language.Syntax
import Language.ListaAsoc

-- Asignación de valores para las variables enteras
type StateI = ListaAsoc VarName Int
-- Asignación de valores para las variables booleanas
type StateB = ListaAsoc VarName Bool

defaultIntValue :: Int
defaultIntValue = 0

defaultBoolValue :: Bool
defaultBoolValue = True

-- Tipo que representa la continuación de un paso de ejecución.
-- Ésta puede ser: Falta ejecutar una sentencia (ToExec), o ya no hay nada por
-- ejecutar (Finish).
data Continuation = ToExec Statement
                  | Finish


-- El estado consta del valor de las variables enteras y las booleanas
type State = (StateI,StateB)


evalIExpr :: IntExpr -> StateI -> Int
evalIExpr (ConstI x) s = x
evalIExpr (VI x) s = case la_buscar s x of
                          Just b -> b
                          Nothing -> defaultIntValue
evalIExpr (Neg x) s = -(evalIExpr x s)
evalIExpr (Plus x y) s = (evalIExpr x s)+(evalIExpr y s)
evalIExpr (Prod x y) s = (evalIExpr x s)*(evalIExpr y s)
evalIExpr (Div x y) s = div (evalIExpr x s) (evalIExpr y s) 
evalIExpr (Mod x y) s = mod (evalIExpr x s) (evalIExpr y s)

-- Para evaluar las expresiones booleanas
-- necesitamos también el estado de variables enteras
-- porque en Equal y Less tenemos subexpresiones enteras.
evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (ConstB b) s = b
evalBExpr (VB v) (si, sb) = case la_buscar sb v of
				 Just e -> e
				 Nothing -> defaultBoolValue
evalBExpr (And b1 b2) s = (evalBExpr b1 s) && (evalBExpr b2 s)
evalBExpr (Or b1 b2) s = (evalBExpr b1 s) || (evalBExpr b2 s)
evalBExpr (Not b) s = not (evalBExpr b s)
evalBExpr (Equal i1 i2) (si, sb) = (evalIExpr i1 si) == (evalIExpr i2 si)
evalBExpr (Less i1 i2) (si, sb) = (evalIExpr i1 si) < (evalIExpr i2 si)


-- Evaluar un paso de ejecución en un programa.
evalStep :: Statement -> State -> (State , Continuation)
evalStep (Skip) s = (s,Finish)
evalStep (AssignB (Var x BoolT) y) (si,sb) = ((si,(la_agregar x (evalBExpr y (si,sb))) sb),Finish)
evalStep (AssignI (Var x IntT) y) (si,sb) = (((la_agregar x (evalIExpr y si) si),sb), Finish)
evalStep (Seq x y) s = case (evalStep x s) of
			    (s1, Finish) -> (s1, ToExec y)
			    (s2, ToExec z) -> (s2, ToExec (Seq z y))			 
evalStep (If ((x,y):xs)) s = case (evalBExpr x s) of
                                True -> evalStep y s
                                False -> evalStep (If xs) s
evalStep (Do x y) s = case (evalBExpr x s) of
                           False -> (s,Finish)
                           True -> (s, ToExec (Seq y (Do x y)))















