{
module Lexer where

import Tokens
}

-- wrapper es para que alex genere el codigo basico del analizador
-- posn es la plantilla especifica de alex para tener a disposicion las filas y columnas del texto
%wrapper "posn"

-- Expresiones
$digit    = [0-9]                     -- Expresion regular para numeros decimales
$alpha    = [a-zA-ZáéíóúÁÉÍÓÚñÑ]      -- Expresion regular para letras de la a a la z, letras de la A a la Z, las vocales acentuadas y ñÑ
$notDash  = [\x00-\x2c \x2e-\xff]     -- Cualquier carácter ASCII excepto el guion (\x2d)
$notId    = [\x00-\x23 \x25-\xff]     -- Cualquier carácter ASCII excepto el dólar (\x24)

tokens :-

  -- Ignorados (Espacios y Comentarios)
  $white+;    -- espacios, tabulaciones y saltos de linea
  "$-" ( $notDash | \x2d+ $notId )* \x2d* "-$" ; -- comentarios

  -- Palabras Clave (Keywords)              -- Aca ignoramos con _ porque el valor es constante y no lo vamos a usar en la impresion posterior
  "create"                            { \pos _ -> Token (getFila pos) (getColumna pos) TkCreate } 
  "while"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkWhile }
  "if"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkIf }
  "bool"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkBool }
  "int"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkInt }
  "bot"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkBot }
  "on"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkOn }
  "activation"                        { \pos _ -> Token (getFila pos) (getColumna pos) TkActivation }
  "store"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkStore }
  "end"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkEnd }
  "execute"                           { \pos _ -> Token (getFila pos) (getColumna pos) TkExecute }
  "activate"                          { \pos _ -> Token (getFila pos) (getColumna pos) TkActivate }

-- Algunos que no estan en el enunciado
  "for"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkFor }
  "else"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkElse }
  "string"                            { \pos _ -> Token (getFila pos) (getColumna pos) TkString }
  "char"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkChar }
  "do"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkDo }
  "array"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkArray }
  "list"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkList }

  -- Literales Booleanos
  "true"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkTrue }
  "false"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkFalse }

  -- Operadores y Separadores
  ","                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkComa }
  "."                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkPunto }
  ":"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkDosPuntos }
  "("                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkParAbre }
  ")"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkParCierra }
  "+"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkSuma }
  "-"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkResta }
  "*"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkMult }
  "/"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkDiv }
  "%"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkMod }
  "/\"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkConjuncion }
  "\/"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkDisyuncion }
  "~"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkNegacion }
  "<"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkMenor }
  "<="                                { \pos _ -> Token (getFila pos) (getColumna pos) TkMenorIgual }
  ">"                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkMayor }
  ">="                                { \pos _ -> Token (getFila pos) (getColumna pos) TkMayorIgual }
  "="                                 { \pos _ -> Token (getFila pos) (getColumna pos) TkIgual }

  -- Literales Numéricos y de Caracteres   -- Aca s puede ser cualquier valor (que haga match) y lo necesitamos para la impresion
  $digit+                             { \pos s -> Token (getFila pos) (getColumna pos) (TkNum (read s)) }       
  \' [^\'] \'                         { \pos s -> Token (getFila pos) (getColumna pos) (TkCaracter (s !! 1)) }

  -- Identificadores (Variables, funciones, etc.)
  -- Empiezan con letra y pueden seguir con letras, números o guion bajo, el * indica que puede ser cualquier cantidad
  $alpha [$alpha $digit \_]*          { \pos s -> Token (getFila pos) (getColumna pos) (TkIdent s) }

  -- Errores Léxicos
  .                                   { \pos s -> Token (getFila pos) (getColumna pos) (TkError (head s)) }

{
-- Funciones auxiliares para extraer fila y columna
getFila :: AlexPosn -> Int
getFila (AlexPn _ f _) = f

getColumna :: AlexPosn -> Int
getColumna (AlexPn _ _ c) = c
}