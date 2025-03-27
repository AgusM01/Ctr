module Eval where 

-- La idea es que el evaluador devuelva una lista de tuplas (Date , Int).
-- Cada tupla establece cuanto es el dinero actual en una fecha dada. 
-- Posteriormente, plotear eso para obtener un gráfico.


import Def 
import Lib
import Monads 
import qualified Data.Map.Strict              as M
import Prelude                                hiding    ( fst 
                                                        , snd 
                                                        )
import Data.Strict.Tuple
import Control.Monad                                    ( liftM
                                                        , ap
                                                        )

-- Entornos
--type EnvDI = M.Map Date Int 
type EnvVC = M.Map Var Contract 
type EnvVD = M.Map Var Date 

-- Cambiar por dos estados y el valor a devolver sea la lista de tuplas date-dinero.
-- Entorno nulo 
initEnvVC :: EnvVC
initEnvVC = M.empty

initEnvVD :: EnvVD 
initEnvVD = M.empty

-- Mónada estado
newtype State a = State { runState :: EnvVC -> EnvVD -> Pair (Pair a EnvVC) EnvVD }

instance Monad State where
    return x = State (\svc svd -> ((x :!: svc) :!: svd)) 
    m >>= f = State (\svc svd -> let ((x :!: svc') :!: svd') = (runState m svc svd) in (runState f x) svc' svd') 

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap


instance MonadState State where 
    lookfor v = State (\svc svd  -> ((lookfor' v svc svd :!: svc) :!: svd))
        where lookfor' v svc sdv = case M.lookup v svc of 
                                        Just x  -> x 
                                        Nothing -> fromJust $ M.lookup v sdv
    update v i = State (\svc svd -> case i of 
                                       D _ _ _ -> ( () :!: svc ) :!: M.insert v i svd
                                       _       -> ( () :!: M.insert v i svc ) :!: svd)

-- Agrega la primera fecha y el monto de dinero actual.
-- Siempre empezamos en 0
addInitDate :: MonadState m => Comm -> m PlotList
addInitDate InitDate d m y = return [(Date d m y, 0)]


--addVal :: MonadState m => PlotList -> m PlotList -> m PlotList
--addVal v m = State ( 

getEnvC :: MonadState m => m EnvVC
getEnvC = State (\svc svd -> return svc)

getEnvD :: MonadState m => m EnvVD
getEnvD = State (\svc svd -> return svd)

-- Evalúa un programa en el estado nulo. 
eval :: Comm -> PlotList        
eval c = fst (fst ( runState (stepCommStar c) initEnvVC initEnvVD))

-- Evalúa múltiples pasos de un comando. Hasta alcanzar un Skip.
-- No devuelve un valor en sí ya que sólo tiene efectos secundarios.
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c = fstComm >>= \c' -> stepCommStar c'

fstComm :: MonadState m => Comm -> m PlotList
fstComm  (Seq initdate@(InitDate d m y) c2) = stepComm c2 initdate  

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> Comm -> m PlotList
stepComm Skip _ = return [] -- Nunca va a ser Skip
stepComm (LetCont v1 ctr ) initdate =  do eva <- evalCtr ctr initdate
                                          update v1 eva 
                                          return [] 
stepComm (LetDate v1 date) _ =  do update v1 date 
                                   return [] 
stepComm (Seq Skip c2) _ = return [] 
stepComm (Seq c1 c2) initdate  = do  sc1 <- stepComm c1 initdate 
                                     return []

evalCtr :: MonadState m => Contract -> Comm -> m PlotList 
evalCtr Zero initdate       = return []
evalCtr (OneV v c) initdate = do date <- lookfor v
                                 if compDates initdate date then return [(v, 1)]
                                    else return []
evalCtr (OneD d c) initdate =  if compDates initdate d then return [(d,1)]
                                    else return [] 
evalCtr (Give c) initdate   = do l <- evalCtr c initdate
                                 return (negatePlotList l) -- Funcion que toma una PlotList y niega todos sus valores.
evalCtr (And c1 c2) initdate = do v1 <- evalCtr c1 initdate
                                  v2 <- evalCtr c2 initDate 
                                  return (v1 ++ v2)
evalCtr (Or c1 c2) initdate = do v1 <- evalCtr c1 initDate 
                                 v2 <- evalCtr c2 initDate
                                 if betterContract v1 v2 then return v1 -- betterContract toma dos listas de valores y verifica si la primera vale mas que la segunda
                                    else return v2 
evalCtr (TruncateV var c) initdate = do d <- lookfor var 
                                        evalCtr (TruncateD d c) initdate 
evalCtr (TruncateD d c) initdate = do v <- evalCtr c initdate
                                      if compDates initdate d then return (truncateCtr v d)  
                                        else return []
evalCtr (Then c1 c2) initdate = do v1 <- evalCtr c1 initdate 
                                   case v1 of 
                                    xs -> return xs 
                                    _  -> (do v2 <- evalCtr c2 initdate 
                                              return v2)
evalCtr (ScaleN i c) initdate = do v <- evalCtr c initdate 
                                   return (scaleCtr i v)
evalCtr (ScaleV var c) initdate = do q <- lookfor var 
                                     evalCtr (ScaleN q c) initdate
evalCtr (VarC var) initdate = do c <- lookfor var 
                                 evalCtr c initdate 
