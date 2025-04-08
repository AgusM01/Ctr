module Def where

import qualified Arr as A

-- Usado para definir la fecha 
type Day = Int
type Month = Int 
type Year = Int 

-- Valores 
type PlotList = A.Arr (Date, Int)

-- Fecha
data Date = D Day Month Year deriving Show 

-- Variables
type Var = String

-- AST del lenguaje
-- Representación de un contrato
-- Mejora a futuro: Poder poner variables en lugar de Contracts.
-- Preguntar si hay una manera cómoda de hacerlo sin que sea verborrágica (agregar 
-- un constructor más poniendo Var en vez de Contract)
data Contract = Zero
                | OneV Var  -- En el caso de poner date como una variable.
                | OneD Date -- En el caso de poner la fecha directamente.
                | Give Contract
                | And Contract Contract 
                | Or Contract Contract
                | TruncateV Var Contract 
                | TruncateD Date Contract 
                | Then Contract Contract
                | Scale Int Contract -- Me permite crear contratos de mayor valor. 
                | VarC Var deriving Show -- Para poder representar contratos con variables.

-- Representación de comandos
data Comm = Skip
            | LetCont Var Contract 
            | LetDate Var Date 
            | Seq Comm Comm  
            | InitDate Day Month Year deriving Show
