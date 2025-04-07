module ArrSeq where

import Seq
import Par
import qualified Arr as A

-- Funcion auxiliar para reduceS y scanS
-- Contrae una secuencia aplicando el operador recibido de par en par sobre los 
-- elementos de la secuencia. Si la cantidad de elementos es impar, deja el ultimo
-- sin operar.
contract :: (a -> a -> a) -> A.Arr a -> A.Arr a
contract f xs = tabulateS g n'
              where
                n = lengthS xs 
                k = n `div` 2 
                n' = if even n then k else k + 1 -- se precalcula la longitud de la siguiente contracción de la secuencia
                g i = if 2 * i + 1 /= n then f (xs A.! (2*i)) (xs A.! (2*i + 1)) -- Si hay al menos 2 elementos, se operan
                                        else (xs A.! (2*i)) -- Si es el último elemento, se copia a la contracción.

-- Funcion auxiliar para scanS
-- Expande una secuencia que fue comprimida y reducida previamente
-- Los elementos que se encuentran en posicion par se mantienen como eran en 
-- la secuencia recibida. 
-- Los elementos de posiciones impares i son los resultados de operar el elemento anterior
-- con el elemento de la posicion 2*i - 1 de la secuencia original recibida.
expand :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expand f ps s = tabulateS g (lengthS s) -- Se aplica una funcion g que segun el indice realiza distintos calculos
                where
                  g i | even i = ps A.! (div i 2) -- si esta en una posicion par, mantiene el valor
                      | otherwise = let (x, y) = ((ps A.! (div i 2)), (s  A.! (i - 1))) -- Si esta en una posicion impar    
                                    in f x y                                         -- realiza la operción necesaria para obtener el valor
                                                                                     

instance Seq A.Arr where
  emptyS = A.empty -- Implementación de la secuencia vacía con arreglos

  singletonS x = Seq.fromList [x] -- Implementación de la secuencia de un solo elemento con arreglos

  lengthS = A.length -- Implementación de la función lenghtS con arreglos

  nthS = (A.!) -- Implementación de la indexación de secuencias con arreglos

  tabulateS = A.tabulate -- Implementación de la funcion tabulateS con arreglos

  mapS f xs = tabulateS (f . (nthS xs)) (lengthS xs) -- Implementación de la función mapS con arreglos

  filterS p xs = joinS $ tabulateS f (lengthS xs)  -- Implementación de la función filterS con arreglos
                where f i = let x = nthS xs i
                            in if p x then singletonS x 
                                      else emptyS
  
  appendS xs ys = joinS $ fromList [xs, ys] -- Implementación de la concatenación de secuencias con arreglos

  takeS xs n = A.subArray 0 n xs -- Implementación de la función takeS con arreglos

  dropS xs n = A.subArray n ((lengthS xs) - n) xs -- Implementación de la funcion dropS con arreglos

  showtS xs | n == 0 = EMPTY
            | n == 1 = ELT (nthS xs 0)
            | otherwise = NODE (takeS xs m) (dropS xs m)
              where
                n = lengthS xs
                m = div n 2

  showlS xs | n == 0 = NIL
            | otherwise = CONS (nthS xs 0) (dropS xs 1)
            where n = lengthS xs

  joinS = A.flatten -- Implementación de la función joinS con arreglos

  -- Implementación de la función reduceS con arreglos 
  reduceS f e xs | n == 0 = e -- reducción de una secuencia vacía, devuelve el elemento 'e'
                 | n == 1 = f e $ nthS xs 0 -- reduccion de una secuencia de un solo elemento
                 | n > 1 = reduceS f e $ contract f xs -- reducción de una secuencia con múltiples elementos,
                  where n = lengthS xs                 -- se aplica la reducción sobre una contracción de la secuencia.
  
  -- Implementación de la función scanS con arreglos
  scanS f e xs | n == 0 = (emptyS, e) -- scanS de una secuencia vacía
               | n == 1 = let x = nthS xs 0 in (singletonS e, f e x) -- scanS de una secuencia con un solo elemento
               | otherwise = let (s', t) = scanS f e $ contract f xs in (expand f s' xs, t) -- scanS de una secuencia con
              where n = lengthS xs                                                          -- múltiples elementos.
                                                                                       
  fromList = A.fromList