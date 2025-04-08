module Eval where 

-- La idea es que el evaluador devuelva una lista de tuplas (Date , Int).
-- Cada tupla establece cuanto es el dinero actual en una fecha dada. 
-- Posteriormente, plotear eso para obtener un gráfico.


import Def 
import Lib
import Monads 
import Data.Maybe
import qualified Data.Map.Strict              as M
import Prelude                                hiding    ( fst 
                                                        , snd 
                                                        )
import Data.Strict.Tuple
import Control.Monad                                    ( liftM
                                                        , ap
                                                        )

import Seq
import Par 
import Arr 
import ArrSeq

-- Entornos
type EnvVC = M.Map Var PlotList 
type EnvVD = M.Map Var Date 

-- Entornos nulo 
initEnvVC :: EnvVC
initEnvVC = M.empty

initEnvVD :: EnvVD 
initEnvVD = M.empty

-- Mónada estado
newtype State a = State { runState :: EnvVC -> EnvVD -> Pair (Pair a EnvVC) EnvVD }

instance Monad State where
    return x = State (\svc svd -> ((x :!: svc) :!: svd)) 
    m >>= f = State (\svc svd -> let ((x :!: svc') :!: svd') = runState m svc svd 
                                 in runState (f x) svc' svd') 

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap


instance MonadState State where 
    
    lookforcontract v = State (\svc svd -> ((lookforcontract' v svc :!: svc) :!: svd))
                where lookforcontract' v svc = fromJust $ M.lookup v svc 

    lookfordate v = State (\svc svd -> ((lookfordate' v svd :!: svc) :!: svd))
        where lookfordate' v svd = fromJust $ M.lookup v svd

    update v i = State (\svc svd -> case i of 
                                       Right d  -> ( () :!: svc ) :!: M.insert v d svd
                                       Left c   -> ( () :!: M.insert v c svc ) :!: svd)

{-
getEnvC :: MonadState m => m EnvVC
getEnvC = State (\svc svd -> return svc)

getEnvD :: MonadState m => m EnvVD
getEnvD = State (\svc svd -> return svd)
-}

-- Evalúa un programa en el estado nulo. 
eval :: Comm -> PlotList        
eval (Seq (initdate@(InitDate d m y)) c) = fst $ fst $ runState (stepCommStar c initdate emptyS) initEnvVC initEnvVD

-- Evalúa múltiples pasos de un comando. Hasta alcanzar un Skip.
-- No devuelve un valor en sí ya que sólo tiene efectos secundarios.
stepCommStar :: MonadState m => Comm -> Comm -> PlotList -> m PlotList
stepCommStar Skip _ pl = return pl
stepCommStar c initdate pl = (stepComm c initdate) >>= \c' -> stepCommStar (fst c') initdate (appendS (snd c') pl) 
                                                                  
-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> Comm -> m (Pair Comm PlotList)
stepComm Skip _ = return (Skip :!: emptyS) -- Si es Skip se agarra en stepCommStar. 
stepComm (LetCont v1 ctr) initdate =  do eva <- evalCtr ctr initdate
                                         update v1 (Left eva) 
                                         return (Skip :!: eva) 
stepComm (LetDate v1 date) _ =  do update v1 (Right date) 
                                   return (Skip :!: emptyS) 
stepComm (Seq Skip c2) _ = return (c2 :!: emptyS) 
stepComm (Seq c1 c2) initdate  = do  sc1 <- stepComm c1 initdate 
                                     return ((Seq (fst sc1) c2) :!: snd sc1)

evalCtr :: MonadState m => Contract -> Comm -> m PlotList 
evalCtr Zero _       = return emptyS
evalCtr (OneV v) initdate = do d <- lookfordate v
                               if (compDates initdate d) then (return (singletonS (d,1)))
                                 else return emptyS
evalCtr (OneD d) initdate =  if (compDates initdate d) then (return (singletonS (d,1)))
                               else return emptyS 
evalCtr (Give c) initdate   = do l <- evalCtr c initdate
                                 return (negatePlotList l) -- Funcion que toma una PlotList y niega todos sus valores.
evalCtr (And c1 c2) initdate = do v1 <- evalCtr c1 initdate
                                  v2 <- evalCtr c2 initdate 
                                  return (appendS v1 v2)
evalCtr (Or c1 c2) initdate = do v1 <- evalCtr c1 initdate 
                                 v2 <- evalCtr c2 initdate
                                 if (betterContract v1 v2) then (return v1) -- betterContract toma dos listas de valores y verifica si la primera vale mas que la segunda
                                    else return v2 
evalCtr (TruncateV var c) initdate = do d <- lookfordate var 
                                        evalCtr (TruncateD d c) initdate 
evalCtr (TruncateD d c) initdate = do v <- evalCtr c initdate
                                      if (compDates initdate d) then (return (truncateCtr v d))  
                                        else return emptyS
evalCtr (Then c1 c2) initdate = do v1 <- evalCtr c1 initdate 
                                   case v1 of 
                                    xs -> return xs 
                                    _  -> (do v2 <- evalCtr c2 initdate 
                                              return v2)
evalCtr (Scale i c) initdate = do v <- evalCtr c initdate 
                                  return (scaleCtr v i)
evalCtr (VarC var) _ = do c <- lookforcontract var 
                          return c
