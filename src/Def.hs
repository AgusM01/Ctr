module Def where

import qualified Arr as A

-- Usado para definir la fecha 
type Day = Int
type Month = Int 
type Year = Int 

-- Valores 
-- Por ahora, solo USD 
--type PlotList = [(Date, Int)] --Currency)]
type PlotList = A.Arr (Date, Int)

-- Inf es para el caso de un horizonte infinito (al momento de crear contrtos One)
data Date = D Day Month Year deriving Show 

-- Monedas disponibles
-- Por ahora sirven solo para mostrarlas. Si un usuario ingresa dos monedas
-- diferentes dará error. 
-- Esto se puede mejorar haciendo que el usuario elija una moneda en específico 
-- y pasando todos los valores a dicha moneda.
--data Currency = GBP | USD | ARS | EUR deriving Show 

{-
--------- VER SI INCLUIR -------------------------------------------------------
-- Las variables observables son aquellas cuyo valor 
-- puede ser determinado a partir de fuentes verificables,
-- como precios de mercado, tasas de interés, índices financieros, entre otros. 
data Obs a = CONST a | LIBOR a  | EUAP a -- | USDEUR a | GBPUSD a | USDAP a 
--------------------------------------------------------------------------------

-- Lo hacemos instancia de Functor para así poder generalizar funciones como lift. 
instance Functor Obs where 
    fmap :: (a -> b) -> Obs a -> Obs b
    fmap f (CONST a) = CONST (f a)
    fmap f (LIBOR a) = LIBOR (f a)
    fmap f (EUAP a)  = EUAP (f a)

-- Lo hacemos instancia de Applicative para generalizar funciones como lift2. 
instance Applicative Obs where 
    pure :: a -> Obs a
    pure = CONST 

    (<*>) :: Obs (a -> b) -> Obs a -> Obs b
    (CONST f) <*> x         = fmap f x 
    (LIBOR f) <*> x         = fmap f x
    (EUAP f) <*>  x         = fmap f x
-}

type Var = String

-- AST del lenguaje
-- Representación de un contrato
-- Mejora a futuro: Poder poner variables en lugar de Contracts.
-- Preguntar si hay una manera cómoda de hacerlo sin que sea verborrágica (agregar 
-- un constructor más poniendo Var en vez de Contract)
data Contract = Zero
                | OneV Var --Currency -- En el caso de poner date como una variable.
                | OneD Date --Currency -- En el caso de poner la fecha directamente.
                | Give Contract
                | And Contract Contract 
                | Or Contract Contract
                | TruncateV Var Contract 
                | TruncateD Date Contract 
                | Then Contract Contract
                | Scale Int Contract -- Me permite crear contratos de mayor valor. <- Analizar observables.
 --               | Get Contract
   --             | Anytime Contract
                | VarC Var deriving Show -- Para poder representar contratos con variables.
  --              | CD Day Month Year deriving Show -- Usado para definir fechas: t1 = date 28 12 2001 

-- Representación de comandos
data Comm = Skip
            | LetCont Var Contract 
            | LetDate Var Date 
            | Seq Comm Comm  
            | InitDate Day Month Year deriving Show

{-
-- Valor del "proceso" (contrato).
-- TimeStep es su horizonte -> Su fecha límite de adquisisión. 
-- Slice es una lista de columnas una por paso de tiempo en orden al revés. 
-- Esto es el valor del proceso cambiando a lo largo del tiempo teniendo
-- como primer elemento su horizonte (cuanto da de por si).

-- Esto se entiende mejor pensando que un contrato que dice 
-- pagar 100usd el 28 de Diciembre del 2025 (su horizonte), puede 
-- pagar menos el tiempo antes de dicha fecha debido a como afectan
-- los intereses. La inflación siempre existe y pagar 100usd en Mayo
-- es totalmente diferente que hacerlo en Diciembre.
type ValProc = (TimeStep, [Slice])
type Slice = Double -- Será un arreglo normal, no un lattice (recombining tree)
type TimeStep = Date

-- ValProc puede ser una mónada que vaya llevando
-- la fecha. 
-- Puede ser mónada estado.
-- newtype ValProc a = VP {runVP :: (TimeStep, )}
-}
