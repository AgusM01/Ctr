module Lib where 

import Def  

import Seq
import Par 
import Arr 
import ArrSeq

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time.Calendar (fromGregorian)

import qualified Arr as A

-- Compara si d1 es anterior a d2. 
-- Es una función parcial. Su dominio es simplemente: InitDate 
compDates :: Comm -> Date -> Bool 
compDates (InitDate d1 m1 y1) (D d2 m2 y2) =    (y1 < y2) || 
                                                (y1 == y2 && m1 < m2) || 
                                                (y1 == y2 && m1 == m2 && d1 <= d2)

-- Compara dos fechas.
compDatesPost :: Date -> Date -> Ordering
compDatesPost (D d1 m1 y1) (D d2 m2 y2) | d1 == d2 && m1 == m2 && y1 == y2 = EQ
                                        | y1 < y2 = LT
                                        | y1 == y2 && (m1 < m2) = LT
                                        | y1 == y2 && m1 == m2 && (d1 < d2) = LT
                                        | y1 > y2 = GT
                                        | y1 == y2 && (m1 > m2) = GT
                                        | y1 == y2 && m1 == m2 && d1 > d2 = GT

ord :: (Date, Int) -> (Date, Int) -> Ordering
ord (d1,i1) (d2,i2) = compDatesPost d1 d2

ordArr :: (Date, A.Arr Int) -> (Date, A.Arr Int) -> Ordering
ordArr (d1,a1) (d2,a2) = compDatesPost d1 d2

-- Checkea si el contrato xs1 da más que el xs2
betterContract :: PlotList -> PlotList -> Bool
betterContract xs1 xs2 = sumPlotList xs1 >= sumPlotList xs2 

-- Suma todos los valores de una PlotList.
sumPlotList :: PlotList -> Int 
sumPlotList s = snd $ reduceS (\(d1,v1) (d2,v2) -> (d2, v1 + v2)) (D 0 0 0, 0) s  

-- Niega todos los valores de una PlotList.
negatePlotList :: PlotList -> PlotList 
negatePlotList s = scaleCtr s (-1) 

-- Escala una PlotList por el valor dado.
scaleCtr :: PlotList -> Int -> PlotList
scaleCtr s v = mapS (\(d,i) -> (d, v*i)) s 

-- Cambia las fechas de una PlotList por la dada.
truncateCtr :: PlotList -> Date -> PlotList 
truncateCtr s nd = mapS (\(_,i) -> (nd,i)) s 

-- Función para unir secuencias.
merge :: (a -> a -> Ordering) -> A.Arr a -> A.Arr a -> A.Arr a 
merge r s1 s2   | lengthS s1 == 0 = s2 
                | lengthS s2 == 0 = s1
                | otherwise = case r (nthS s1 0) (nthS s2 0) of
                                    LT -> appendS (singletonS (nthS s1 0)) (merge r (dropS s1 1) s2)
                                    _ ->  appendS (singletonS (nthS s2 0)) (merge r s1 (dropS s2 1))
                     
-- Función para ordenar secuencias.
sort :: (a -> a -> Ordering) -> A.Arr a -> A.Arr a
sort re s = case showtS s of
            EMPTY -> emptyS
            ELT a -> singletonS a
            NODE l r -> merge re (sort re l) (sort re r)

-- Función para poder unir los montos que tienen misma fecha.
collect :: A.Arr (Date,Int) -> A.Arr (Date, A.Arr Int)
collect s = let sOrd = sort ord s
                s' = mapS (\(a,b) -> (a, singletonS b)) sOrd
                (x,y) =  (scanS fun (nthS s' 0) (dropS s' 1))
                sParseada = appendS x (singletonS y)
                sFinal = group ordArr sParseada
            in sFinal

fun :: (Date, A.Arr Int) -> (Date, A.Arr Int) -> (Date, A.Arr Int)
fun (a1,sb1) (a2,sb2)   | compDatesPost a1 a2 == EQ = (a1, appendS sb1 sb2)
                        | otherwise = (a2, sb2)

group :: (a -> a -> Ordering) -> A.Arr a -> A.Arr a 
group f s = let tr = showtS s 
            in case tr of  
                EMPTY -> emptyS
                ELT a -> singletonS a 
                NODE l r -> let (l', r') = (group f l) ||| (group f r)
                            in if (f (nthS l' ((lengthS l') - 1)) (nthS r' 0)) == EQ then appendS (takeS l' (lengthS l' - 1)) r'
                                    else (appendS l' r')

finalSeq :: A.Arr (Date, Int) -> A.Arr (Date, Int)
finalSeq s = let (x,y) = scanS f (nthS s 0) (dropS s 1)
             in appendS x (singletonS y)
        where f (d1,i1) (d2,i2) = (d2, i1 + i2)

postProcess :: PlotList -> A.Arr (Date, Int)
postProcess pl = (finalSeq (mapS (\(d,s) -> (d, (reduceS (+) 0 s))) (collect pl)))

-- Función que permite plottear.
finalPlot :: A.Arr (Date, Int) -> IO()
finalPlot s = toFile def "Historial.png" $ do 
        layout_title .= "Historial"
        let dateS = mapS (\(D d m y, i) -> ([(fromGregorian (toInteger y) m d)], [i])) s
            (x,y) = scanS (\(x1,y1) (x2,y2) -> (x1 ++ x2, y1 ++ y2)) (nthS dateS 0) (dropS dateS 1)
            datos = zip (fst y) (snd y)
        plot (line "Dinero - USD" [datos])

