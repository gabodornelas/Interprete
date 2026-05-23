module Tokens where

-- Definimos alias de tipos para mayor claridad conceptual
type Fila    = Int
type Columna = Int

data TokenClass
  -- Palabras Clave (Keywords) presentes en el documento del enunciado del proyecto
  = TkCreate            -- Palabra clave para crear (supongo, aunque en este punto su utilidad no es relevante, la idea de estos comentarios es rellenarlos cuando haya mas contexto en la etapa2)
  | TkWhile             -- Palabra clave para hacer un bucle
  | TkIf                -- Palabra clave para agregar una condicion
  | TkBool              -- Palabra clave de Tipo de dato booleano
  | TkInt               -- Palabra clave de Tipo de dato entero
  | TkBot               -- Palabra clave para ?
  | TkOn                -- Palabra clave para ?
  | TkActivation        -- Palabra clave para ?
  | TkStore             -- Palabra clave para guardar un dato
  | TkEnd               -- Palabra clave para terminar un bloque de codigo
  | TkExecute           -- Palabra clave para ejecutar una funcion
  | TkActivate          -- Palabra clave para ?

 -- Algunas ausentes en el documento de enunciado del proyecto que me parecieron importantes de incluir
  | TkFor               -- Palabra clave para hacer un bucle
  | TkElse              -- Palabra clave para encapsular el camino contrario de no cumplirse la condicion if
  | TkString            -- Palabra clave de Tipo de dato String (cadena de caracteres)
  | TkChar              -- Palabra clave de Tipo de dato Caracter
  | TkDo                -- Palabra clave para complementar alguna de bucles
  | TkArray             -- Palabra clave de Tipo de dato Arreglo
  | TkList              -- Palabra clave de Tipo de dato Lista

  -- Identificadores (Nombres de variables, funciones, etc.)
  | TkIdent String

  -- Literales
  | TkNum Int          -- Literales numéricos enteros
  | TkTrue             -- Literal booleano verdadero
  | TkFalse            -- Literal booleano falso
  | TkCaracter Char    -- Literales de caracteres (ej. 'a')

  -- Operadores y Separadores
  | TkComa
  | TkPunto
  | TkDosPuntos
  | TkParAbre
  | TkParCierra
  | TkSuma
  | TkResta
  | TkMult
  | TkDiv
  | TkMod
  | TkConjuncion
  | TkDisyuncion
  | TkNegacion
  | TkMenor
  | TkMenorIgual
  | TkMayor
  | TkMayorIgual
  | TkIgual

  -- Aqui faltaria corchetes llaves, ampersand, puntoYcoma, etc (que no fueron mencionados para esta etapa)

  | TkError Char        -- Characteres no definidos
  deriving (Eq)

-- Definimos como se muestra en salida estandar
instance Show TokenClass where
    show TkCreate       = "TkCreate"
    show TkWhile        = "TkWhile"
    show TkIf           = "TkIf"
    show TkBool         = "TkBool"
    show TkInt          = "TkInt"
    show TkBot          = "TkBot"
    show TkOn           = "TkOn"
    show TkActivation   = "TkActivation"
    show TkStore        = "TkStore"
    show TkEnd          = "TkEnd"
    show TkExecute      = "TkExecute"
    show TkActivate     = "TkActivate"
    show TkFor          = "TkFor"
    show TkElse         = "TkElse"
    show TkString       = "TkString"
    show TkChar         = "TkChar"
    show TkDo           = "TkDo"
    show TkArray        = "TkArray"
    show TkList         = "TkList"

    show (TkIdent id)   = "TkIdent(\"" ++ id ++ "\")"           -- Para mostrar el valor entre parentesis
    show (TkNum n)      = "TkNum(" ++ show n ++ ")"             -- Para mostrar el valor entre parentesis
    show TkTrue         = "TkTrue"
    show TkFalse        = "TkFalse"
    show (TkCaracter c) = "TkCaracter('" ++ [c] ++ "')"         -- Para mostrar el valor entre parentesis
    
    show TkComa         = "TkComa"
    show TkPunto        = "TkPunto"
    show TkDosPuntos    = "TkDosPuntos"
    show TkParAbre      = "TkParAbre"
    show TkParCierra    = "TkParCierra"
    show TkSuma         = "TkSuma"
    show TkResta        = "TkResta"
    show TkMult         = "TkMult"
    show TkDiv          = "TkDiv"
    show TkMod          = "TkMod"
    show TkConjuncion   = "TkConjuncion"
    show TkDisyuncion   = "TkDisyuncion"
    show TkNegacion     = "TkNegacion"
    show TkMenor        = "TkMenor"
    show TkMenorIgual   = "TkMenorIgual"
    show TkMayor        = "TkMayor"
    show TkMayorIgual   = "TkMayorIgual"
    show TkIgual        = "TkIgual"

-- El tipo principal
data Token = Token Fila Columna TokenClass
  deriving (Eq, Show)
