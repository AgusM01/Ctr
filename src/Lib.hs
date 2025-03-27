module Lib where 

import Def  

-- Compara si d1 es anterior a d2. 
-- Es una función parcial. Su dominio es simplemente: InitDate 
compDates :: Comm -> Date -> Bool 
compDates (InitDate d1 m1 y1) (D d2 m2 y2) =    (y1 < y2) || 
                                                (y1 == y2 && m1 < m2) || 
                                                (y1 == y2 && m1 == m2 && d1 <= d2)

betterContract :: PlotList -> PlotList -> Bool
betterContract _ [] = True
betterContract [] _ = False 
betterContract xs1 xs2 = sumPlotList xs1 >= sumPlotList xs2 

sumPlotList :: PlotList -> Int 
sumPlotList [] = 0
sumPlotList ((d,i):xs) = i + sumPlotList xs

negatePlotList :: PlotList -> PlotList 
negatePlotList [] = []
negatePlotList ((d,i):xs) = (d,-i):negatePlotList xs

scaleCtr :: PlotList -> Int -> PlotList
scaleCtr [] _ = []
scaleCtr ((d,i):xs) s = (d,s*i):scaleCtr xs s

truncateCtr :: PlotList -> Date -> PlotList 
truncateCtr [] _ = []
truncateCtr ((d,i):xs) nd = (nd,i):truncateCtr xs nd 
