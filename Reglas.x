{
module Reglas where

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
  \$\$ .* ;                                     -- comentario de una linea

  -- Palabras Clave (Keywords)              -- Aca ignoramos con _ porque el valor es constante y no lo vamos a usar en la impresion posterior
  "create"                            { \pos _ -> Token (getFila pos) (getColumna pos) TkCreate } 
  "bot"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkBot }
  "on"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkOn }
  "activation"                        { \pos _ -> Token (getFila pos) (getColumna pos) TkActivation }
  "end"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkEnd }
  "execute"                           { \pos _ -> Token (getFila pos) (getColumna pos) TkExecute }
  "activate"                          { \pos _ -> Token (getFila pos) (getColumna pos) TkActivate }
  "advance"                           { \pos _ -> Token (getFila pos) (getColumna pos) TkAdvance }
  "deactivate"                        { \pos _ -> Token (getFila pos) (getColumna pos) TkDeactivate }
  "deactivation"                      { \pos _ -> Token (getFila pos) (getColumna pos) TkDeactivation }

  "if"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkIf }
  "else"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkElse }
  "while"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkWhile }

  "store"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkStore }
  "collect"                           { \pos _ -> Token (getFila pos) (getColumna pos) TkCollect }
  "as"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkAs }
  "drop"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkDrop }
  "left"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkLeft }
  "right"                             { \pos _ -> Token (getFila pos) (getColumna pos) TkRight }
  "up"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkUp }
  "down"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkDown }
  "read"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkRead }
  "send"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkSend }
  "default"                           { \pos _ -> Token (getFila pos) (getColumna pos) TkDefault }

  "me"                                { \pos _ -> Token (getFila pos) (getColumna pos) TkMe }
  "bool"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkBool }
  "int"                               { \pos _ -> Token (getFila pos) (getColumna pos) TkInt }
  "char"                              { \pos _ -> Token (getFila pos) (getColumna pos) TkChar }

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
  "/="                                { \pos _ -> Token (getFila pos) (getColumna pos) TkDesigual }

  -- Literales Numéricos y de Caracteres   -- Aca s puede ser cualquier valor (que haga match) y lo necesitamos para la impresion
  $digit+                             { \pos s -> Token (getFila pos) (getColumna pos) (TkNum (read s)) }       
  \' [^\'] \'                         { \pos s -> Token (getFila pos) (getColumna pos) (TkCaracter (s !! 1)) }
  -- Caracteres especiales
  \' \\ n \'          { \pos _ -> Token (getFila pos) (getColumna pos) (TkCaracter '\n') }
  \' \\ t \'          { \pos _ -> Token (getFila pos) (getColumna pos) (TkCaracter '\t') }
  \' \\ \' \'         { \pos _ -> Token (getFila pos) (getColumna pos) (TkCaracter '\'') }

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