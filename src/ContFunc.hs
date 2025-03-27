module ContFunc where 

import Def 
import Prelude hiding (truncate, or)

-- El contrato zero puede ser adquirido 
-- en cualquier momento. No tiene derechos 
-- ni obligaciones y tiene un horizonte 
-- infinito.
zero :: Contract 
zero = Zero

-- (one k) es un contrato que inmediatamente
-- paga al tenedor una unidad de la moneda k.
-- El contrato tiene horizonte infinito. 
one :: Currency -> Contract 
one = One Inf   

-- Adquirir (give c) es adquirir todos los
-- derechos de c como obligaciones, y vice versa.
-- Notar que para un contrato bilateral q
-- entre partes A y B, A adquiriendo q implica
-- que B adquiere (give q). 
give :: Contract -> Contract 
give = Give 

-- Si se adquiere (and c1 c2) entonces inmediatamente
-- se adquiere ambos c1 (a menos que haya expirado)
-- y c2 (a menos que haya expirado). El contrato compuesto
-- expira cuando ambos c1 y c2 expiran.  
and :: Contract -> Contract -> Contract 
and = And 

-- Si se adquiere (or c1 c2) se debe adquirir inmediatamente
-- c1 o c2 (no ambos). Si alguno expiró, ese no se podrá
-- elegir. Cuando ambos hayan expirado, el contrato compuesto
-- expira. 
or :: Contract -> Contract -> Contract 
or = Or 

-- (truncate t c) es exactamente como c excepto
-- que expira en la fecha t, o el horizonte de c,
-- lo que ocurra primero. 
-- Notar que truncate limita solo el posible 
-- fecha de adquisición de c; NO limita los 
-- derechos u obligaciones de c, los cuales
-- se pueden extender bastante mas allá de c. 
truncate :: Date -> Contract -> Contract 
truncate = Truncate

-- Si se adquiere (then c1 c2) y c1 no expiró,
-- se adquiere c1. Si c1 expiró, pero c2 no, se 
-- adquiere c2. El contrato compuesto expira cuando 
-- ambos c1 y c2 hayan expirado. 
cthen :: Contract -> Contract -> Contract 
cthen = Then 

-- Si se adquiere (scale o c), entonces se adquiere
-- c en el mismo momento, excepto que todos los derechos
-- y obligaciones de c son multiplicados por el valor
-- del observable o en el momento de su adquisisión. 
-- (El observable o podria ser cuando vale el dolar en 
-- ese momento).
scale :: Obs Double -> Contract -> Contract 
scale = Scale 

-- Si se adquiere (get c) entonces se debe adquirir c
-- en su fecha de expiración. El contrato compuesto,
-- expira al momento en que c expira. 
get :: Contract -> Contract 
get = Get 

-- Si se adquiere (anytime c) entonces se debe 
-- adquirir c, pero se puede hacer en cualquier momento
-- entre el tiempo de adquicisión de (anytime c) y
-- la fecha de expiración de c. El contrato
-- compuesto expira cuando c lo hace. 
anytime :: Contract -> Contract 
anytime = Anytime

