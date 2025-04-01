module Main (main) where

import            System.Console.GetOpt
import            Parser                ( parseComm )
import            Lib
import qualified  System.Environment    as Env 
import qualified  Eval                  as E

-------------------------------------------------------

data Options = Options
    { optAST  :: Bool,
      optEval :: PlotList,
      optHelp :: Bool
    } deriving Show

defaultOptions :: Options 
defaultOptions = Options { optAST = False, optEvaL = [], optHelp = False }

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


 
        




main :: IO ()
main = ...
