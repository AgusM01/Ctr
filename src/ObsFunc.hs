module ObsFunc where
import Def 

-- (konst x) es un observable que 
-- tiene el valor x en todo momento. 
konst :: a -> Obs a 
konst = CONST 

-- (lift f o) es el observable cuyo valor
-- es el resultado de aplicar f 
-- al valor del observable o. 
-- PREGUNTAR USO DE MÓNADAS.
lift :: (a -> b) -> Obs a -> Obs b 
lift = fmap

-- (lift2 f o1 o2) es el observable
-- cuyo valor es el resultado de aplicar 
-- f a los alores de los observables
-- o1 y o2. 

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c 
lift2 f o1 o2 = f  <$> o1 <*> o2 

-- Todas las operaciones numéricas se
-- elevan al tipo Obs. 
instance Num a => Num (Obs a) where 
    
    (+) :: Num a => Obs a -> Obs a -> Obs a
    (+) = lift2 (+)

    (*) :: Num a => Obs a -> Obs a -> Obs a
    (*) = lift2 (*)

    abs :: Num a => Obs a -> Obs a 
    abs = lift abs 

    signum :: Num a => Obs a -> Obs a 
    signum = lift signum 

    fromInteger :: Num a => Integer -> Obs a 
    fromInteger i = pure (fromInteger i) 

    negate :: Num a => Obs a -> Obs a 
    negate = lift negate

-- El valor del observable (time t) en el tiempo
-- s, es el número de dias entre s y t, positivo si
-- s es mas tardío que t. 
-- COMO OBTENGO S? => MÓNADAS?
-- time :: Date -> Obs Days 

