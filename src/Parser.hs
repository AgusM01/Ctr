module Parser where
{-# HLINT ignore "Use <$>" #-}

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Def 


totParser :: Parser a -> Parser a 
totParser p = do whiteSpace ctr 
                 t <- p 
                 eof 
                 return t 

-- Analizador de tokens
ctr :: TokenParser u 
ctr = makeTokenParser
    (emptyDef
        { commentStart = "/*"                                                  ,
          commentEnd = "*/"                                                    ,
          commentLine = "//"                                                   ,
          opLetter = char '='                                                  ,
          reservedNames = [ "zero", "one", "date", {-"USD", "EUR", "ARS",-} "skip"], 
          reservedOpNames = [ "give", "and", "or", "truncate", 
                            "then", "scale", {-"get",-} 
                            {-"anytime,"-}"=", ";" ]
        }
    )

-- Desambigüamos la gramática:
-- intexp ::= intexp '+' iterm | intexp '-' iterm | iterm 
-- iterm   ::= iterm '*' minus | iterm '/' minus | minus  
-- minus  ::= '-u' mm | mm  
-- mm     ::= nat | var | var '++' | var '--' | '('intexp')'  

 
-- Escribimos ahora la gramática del lenguaje
-- Para poner fechas t valores, estos deben estar previamente definidos
-- en una variable. 

-- letter :: 'a' | ... | '>'  
-- digit ::= '0' | ... | '9'
-- nat ::= digit | digit nat  
-- var ::= letter | letter var

-- CommD = initDate ';' Comm
-- Comm ::=  var '=' ContExp | Comm ';' Comm | var '=' Date | 'skip' 

-- Date ::=  'date' num num num

-- Por ahora solo USD
-- Cur ::= "GBP" | "USD" | "ARS" | "EUR"

-- ContExp1 ::=  ContExp1 'and' ContExp2 
--              | ContExp1 'or' ContExp2 
--              | ContExp1 'then' ContExp2
--              | ContExp2

-- ContExp2 ::= zero 
--              | one var cur -> Cur por ahora solo USD 
--              | one date cur -> Cur por ahora solo USD
--              |'give' ContExp1 
--              | 'truncate' var ContExp1
--              | 'scale' var ContExp1  
--              | 'anytime' ContExp1
--              | var
--              | '(' ContExp1 ')'


contexp1 :: Parser Contract
contexp1 = chainl1 contexp2 op1Parser

op1Parser :: Parser (Contract -> Contract -> Contract)
op1Parser = try (do reservedOp ctr "and" 
                    return And)
                <|> try (do reservedOp ctr "or"
                            return Or)
                        <|> (do reservedOp ctr "then"
                                return Then)

contexp2 :: Parser Contract
contexp2 = try primParser <|> op2Parser 
        
op2Parser :: Parser Contract
op2Parser = try giveParser <|> 
            try truncateParser <|> 
            try scaleParser <|> 
           -- try anytimeParser <|>
            varCParser

varCParser :: Parser Contract
varCParser = do v <- varParser 
                return $ VarC v

primParser :: Parser Contract
primParser = try zeroParser <|>
             try oneParser <|>
             parensParser


zeroParser :: Parser Contract 
zeroParser = do reserved ctr "zero"
                return Zero

oneParser :: Parser Contract
oneParser = try (do reserved ctr "one"
                    v <- varParser 
                    --c <- curParser 
                    return $ OneV v)
                <|> do reserved ctr "one"
                       d <- dateParser
                       --c <- curParser
                       return $ OneD d

parensParser :: Parser Contract 
parensParser = do symbol ctr "("
                  c <- contexp1
                  symbol ctr ")"
                  return c 

giveParser :: Parser Contract
giveParser = do reservedOp ctr "give"
                c <- contexp1
                return $ Give c 

truncateParser :: Parser Contract
truncateParser = try (do reservedOp ctr "truncate"
                         v <- varParser
                         c <- contexp1 
                         return (TruncateV v c))
                      <|> do reservedOp ctr "truncate"
                             d <- dateParser 
                             c <- contexp1
                             return (TruncateD d c)

scaleParser :: Parser Contract
scaleParser = try (do reservedOp ctr "scale"
                      n <- natural ctr
                      c <- contexp1
                      return (ScaleN (fromInteger n) c))
                 <|> do reservedOp ctr "scale"
                        v <- varParser
                        c <- contexp1 
                        return (ScaleV v c)
{-
anytimeParser :: Parser Contract
anytimeParser = do reservedOp ctr "anytime"
                   c <- contexp1
                   return $ Anytime c


curParser :: Parser Currency 
curParser = try (do reserved ctr "GBP"
                    return GBP)
                <|> try (do reserved ctr "USD"
                            return USD)
                        <|> try (do reserved ctr "ARS"
                                    return ARS)
                                 <|> do reserved ctr "EUR"
                                        return EUR
-}

dateParser :: Parser Date 
dateParser = do d <- natural ctr
                m <- natural ctr
                y <- natural ctr 
                return $ D (fromInteger d) (fromInteger m) (fromInteger y)

varParser :: Parser Var
varParser = do identifier ctr 

-- Puede aparecer un skip en cualquier momento.
-- No importa, se ajusta en el eval.
commParser :: Parser Comm 
commParser = do d <- commDateParser 
                reservedOp ctr ";"
                r <- chainl1 (try letContParser <|> try letDateParser <|> skipParser) seqParser
                return $ Seq d r 

skipParser :: Parser Comm
skipParser = do reserved ctr "skip"
                return Skip

letContParser :: Parser Comm 
letContParser = do v <- varParser
                   reservedOp ctr "="
                   c <- contexp1
                   return $ LetCont v c 

letDateParser :: Parser Comm 
letDateParser = do v <- varParser 
                   reservedOp ctr "="
                   d <- dateParser
                   return $ LetDate v d 

seqParser :: Parser (Comm -> Comm -> Comm) 
seqParser = do reservedOp ctr ";"
               return Seq

commDateParser :: Parser Comm
commDateParser = do reserved ctr "date"
                    d <- natural ctr
                    m <- natural ctr
                    y <- natural ctr 
                    return $ InitDate (fromInteger d) (fromInteger m) (fromInteger y)
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm 
parseComm = parse (totParser commParser)

