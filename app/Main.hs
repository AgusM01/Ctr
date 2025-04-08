module Main (main) where

import            System.Console.GetOpt
import            Parser                ( parseComm )
import            Lib
import            System.Exit
import            Control.Monad
import qualified  System.Environment    as Env 
import qualified  Eval                  as E

import Seq
import Par 
import Arr 
import ArrSeq

import qualified Arr as A
-------------------------------------------------------

data Options = Options -- Define las opciones utilizando un registro.
    { optAST  :: Bool,
      optEval :: Bool,
      optHelp :: Bool
    } deriving Show

defaultOptions :: Options 
defaultOptions = Options { optAST = False, optEval = False, optHelp = False }

-- Cada función sirve para actualizar el campo:
-- (\opts -> opts { optAST = True }) (Options False [] False) 
-- (Options False [] False) { optAST = True }
--  En Haskell, la notación de actualización de registros { campo = nuevoValor } crea una nueva estructura con el campo modificado.
-- Esto significa que el nuevo valor es:
-- Options { optAST = True, optEval = [], optHelp = False }
options :: [OptDescr (Options -> Options)]
options =
    [ Option    ['a']
                ["AST"]
                (NoArg (\opts -> opts { optAST = True }))
                "Mostrar el AST del programa de entrada."
    , Option    ['e']
                ["evaluator"]
                (NoArg (\opts -> opts { optEval = True }))
                "Evaluar el programa de entrada."
    , Option    ['h']
                ["help"]
                (NoArg (\opts -> opts { optHelp = True }))
                "Imprimir guia de uso."
  ]


-- getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
-- [a] es una lista de funciones que transforman la configuración del programa.
-- (cuando a es de tipo (Options -> Options)
-- [String]: Argumentos que no fueron reconocidos como opciones.
-- [String]: Lista de errores si hubo problemas con la sintaxis de las opciones.
-- 
-- Permute permite que las opciones aparezcan en cualquier orden.
-- flip intercambia los argumentos de una funcion binaria: flip f x y = f y x 
-- foldl (flip id) defaultOptions o = (flip id) Options { optAST = False, optEvaL = False, optHelp = False } (\opts -> opts { optAST = True })
-- foldl (flip id) defaultOptions o = id (\opts -> opts { optAST = True }) (Options { optAST = False, optEvaL = False, optHelp = False })
-- y así le va cambiando las opciones.
-- Finalmente encapsula todo en IO.
-- usageInfo: Return a string describing the usage of a command, derived from the header (first argument) and the options described by the second argument.
finalOptions :: [String] -> IO (Options, [String])
finalOptions argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Uso:"

-- Computation getArgs returns a list of the program's command line arguments (not including the program name).
main :: IO ()
main = do 
    s : opts <- Env.getArgs 
    (opts', _) <- finalOptions opts
    runOptions s opts'

runOptions :: FilePath -> Options -> IO()
runOptions fp opts 
    | optHelp opts = putStrLn (usageInfo "Uso: " options)
    | otherwise = do 
      s <- readFile fp 
      case parseComm fp s of
        Left error -> print error 
        Right ast -> if  
            | optAST opts   -> print ast 
            | optEval opts  -> finalPlot (postProcess (E.eval ast))
            | otherwise     -> print ast 



