module Monads where 

import Def 

-- Clases de mónadas que proveen las operaciones necesarias
-- para implementar el evaluador.

-- Clase para representar mónadas con estado de variables 
class Monad m => MonadState m where 

    -- Busca el contrato asociado a la variable dada. 
    lookforcontract :: Var -> m PlotList 

    -- Busca la fecha asociada a la variable dada.
    lookfordate :: Var -> m Date

    -- Cambia el valor de una fecha 
    update :: Var -> Either PlotList Date -> m ()


