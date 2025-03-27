module Monads where 

import Def 

-- Clases de mónadas que proveen las operaciones necesarias
-- para implementar el evaluador.

-- Clase para representar mónadas con estado de variables 
class Monad m => MonadState m where 
    -- Busca el monto de dinero en x fecha. 
    lookfor :: Date -> m Int 
    -- Cambia el valor de una fecha 
    update :: Date -> Int -> m ()


