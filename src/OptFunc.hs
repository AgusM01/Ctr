-- Ver de implementar, quizas sea mejor dejarlo afuera.
module OptFunc where 

import ContFunc

import Def ( Contract(..), Obs, Currency, Date )
import Prelude hiding (truncate, or)

-- Una opción europea da el derecho a elegir, 
-- en una fecha en particular, adquirir 
-- o no el contrato subyacente. 

-- c5 = european (date "24 Apr 2003") (
--          zcb (date "12 May 2003") 0.4
--          GBP `and`
--          zcb (date "12 May 2004") 9.3
--          GBP `and`
--          zcb (date "12 May 2005") 109.3 GBP `and`
--          give (z b (date "26 Apr 2003") 100 GBP)
--      )

-- Este contrato da el derecho a elegir,
-- el 24 de Abril del 2003, si adquirir o no
-- un contrato subyacente que consiste de 3 recibos 
-- y un pago. 

-- En la industria financiera, este tipo de contrato 
-- se denomina compra de un bono con cupón, 
-- que otorga el derecho, en una fecha futura, 
-- a comprar un bono por un precio prescrito.

european :: Date -> Contract -> Contract 
european t u = get (truncate t (or u zero))

-- Encapsulamos el patrón comun de
-- en una fecha dada elegir entre un contrato
-- t o nada.
perhaps :: Date -> Contract -> Contract 
perhaps t u = truncate t (or u zero)


-- Una opción americana ofrece más
-- flexibilidad que una opción europea. Tipicamente,
-- una opción americana confiere el derecho a adquirir
-- un contrato subyacente en cualquier tiempo entre dos fechas,
-- o no adquirirlo en absoluto. 

american :: (Date, Date) -> Contract -> Contract
american (t1,t2) u = get $ cthen (truncate t1 opt) opt 
                        where opt :: Contract 
                              opt = anytime (perhaps t2 u) 