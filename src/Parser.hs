module Parser where
{-# HLINT ignore "Use <$>" #-}

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Def 

import Data.Time.Calendar (fromGregorian, fromGregorianValid)

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
          reservedNames = [ "zero", "one", "date", "skip"], 
          reservedOpNames = [ "give", "and", "or", "truncate", 
                            "then", "scale",  
                            "=", ";" ]
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

-- letter ::= 'a' | ... | 'z'  
-- digit ::= '0' | ... | '9'
-- nat ::= digit | digit nat  
-- var ::= letter | letter var

-- CommInit = initDate ';' Comm
-- Comm ::=  var '=' ContExp1 | Comm ';' Comm | var '=' Date | 'skip' 

-- Date ::=  'date' nat nat nat

-- ContExp1 ::=  ContExp1 'and' ContExp2 
--              | ContExp1 'or' ContExp2 
--              | ContExp1 'then' ContExp2
--              | ContExp2

-- ContExp2 ::= zero 
--              | one var 
--              | one date 
--              | 'give' ContExp1
--              | 'truncate' date ContExp1
--              | 'truncate' var ContExp1
--              | 'scale' nat ContExp1  
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
                    return $ OneV v)
                <|> do reserved ctr "one"
                       d <- dateParser
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
scaleParser = do reservedOp ctr "scale"
                 n <- natural ctr
                 c <- contexp1
                 return (Scale (fromInteger n) c)

dateParser :: Parser Date 
dateParser = do d <- natural ctr
                m <- natural ctr
                y <- natural ctr
                case fromGregorianValid y (fromInteger m) (fromInteger d) of 
                    Just _ -> return $ D (fromInteger d) (fromInteger m) (fromInteger y)
                    Nothing -> fail "Fecha inválida."

varParser :: Parser Var
varParser = do identifier ctr 

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
                    case fromGregorianValid y (fromInteger m) (fromInteger d) of 
                        Just _  -> return $ InitDate (fromInteger d) (fromInteger m) (fromInteger y)
                        Nothing -> fail "Fecha inicial invalida." 
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm 
parseComm = parse (totParser commParser)
-- parse p filePath input runs a parser p over Identity without user state. The filePath is only used in error messages and may be the empty string. Returns either a ParseError (Left) or a value of type a (Right).
