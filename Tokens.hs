module Tokens where

-- Definimos alias de tipos para mayor claridad conceptual
type Fila    = Int
type Columna = Int

data TokenClass
  -- Palabras Clave (Keywords) presentes en el documento del enunciado del proyecto
  = TkCreate            -- Palabra clave para creacion de robots
  | TkBot               -- Palabra clave para definir a un robot
  | TkOn                -- Palabra clave para ?
  | TkActivation        -- Palabra clave para ejecuta el codigo correspondiente
  | TkEnd               -- Palabra clave para indicar el fin del controlador
  | TkExecute           -- Palabra clave para indicar el principio del controlador
  | TkActivate          -- Palabra clave para activar todos los robots cuyos nombres aparezcan en la ⟨Lista de Identificadores⟩.
  | TkAdvance           -- Palabra clave ejecutar un paso de todos los robots cuyos nombres aparezcan en la ⟨Lista de Identificadores⟩.
  | TkDeactivate        -- Palabra clave para desactivar todos los robots cuyos nombres aparezcan en la ⟨Lista de Identificadores⟩.
  | TkDeactivation      -- Palabra clave para ejecuta el codigo correspondiente

  | TkIf                -- Palabra clave para agregar una condicion
  | TkElse              -- Palabra clave para encapsular el camino contrario de no cumplirse la condicion if
  | TkWhile             -- Palabra clave para hacer un bucle

  | TkStore             -- Palabra clave para evaluar la expresión en ⟨Expresión⟩ y almacenar el valor resultante como valor asociado del robot.
  | TkCollect           -- Palabra clave para tomar el valor que se encuentra almacenado en la posición de la matriz en la que el robot se encuentre.
  | TkReceive           -- Palabra clave para lectura de un dato
  | TkAs                -- Palabra clave para declarar una variable cuyo nombre será ⟨Identificador⟩ y cuyo valor recopilado se almacenará en ella.  
  | TkDrop              -- Palabra clave para evaluar la expresión en ⟨Expresión⟩ y almacenar el valor resultante en la matriz, en la posición actual del robot.
  | TkLeft              -- Palabra clave para indicar el movimiento del robot a la izquierda.
  | TkRight             -- Palabra clave para indicar el movimiento del robot a la derecha.
  | TkUp                -- Palabra clave para indicar el movimiento del robot arriba.
  | TkDown              -- Palabra clave para indicar el movimiento del robot abajo.
  | TkRead              -- Palabra clave para leer un valor de la entrada
  | TkSend              -- Palabra clave para escribir instrucciones en salida
  | TkDefault           -- Palabra clave para ejecutar el comportamiento asociado cuando el robot en cuestión está siendo avanzado y ninguna de las expresiones anteriores se cumple.  


  | TkBool              -- Palabra clave de Tipo de dato booleano
  | TkInt               -- Palabra clave de Tipo de dato entero
  | TkChar              -- Palabra clave de Tipo de dato Caracter

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

  | TkError Char        -- Characteres no definidos
  deriving (Eq)

-- Definimos como se muestra en salida estandar
instance Show TokenClass where
    -- Palabras Clave de Creacion y Control
    show TkCreate       = "TkCreate"
    show TkBot          = "TkBot"
    show TkOn           = "TkOn"
    show TkActivation   = "TkActivation"
    show TkEnd          = "TkEnd"
    show TkExecute      = "TkExecute"
    show TkActivate     = "TkActivate"
    show TkAdvance      = "TkAdvance"
    show TkDeactivate   = "TkDeactivate"
    show TkDeactivation = "TkDeactivation"

    -- Palabras Clave de Flujo
    show TkIf           = "TkIf"
    show TkElse         = "TkElse"
    show TkWhile        = "TkWhile"

    -- Instrucciones de Robot
    show TkStore        = "TkStore"
    show TkCollect      = "TkCollect"
    show TkReceive      = "TkReceive"
    show TkAs           = "TkAs"
    show TkDrop         = "TkDrop"
    show TkLeft         = "TkLeft"
    show TkRight        = "TkRight"
    show TkUp           = "TkUp"
    show TkDown         = "TkDown"
    show TkRead         = "TkRead"
    show TkSend         = "TkSend"
    show TkDefault      = "TkDefault"

    -- Tipos de Datos
    show TkBool         = "TkBool"
    show TkInt          = "TkInt"
    show TkChar         = "TkChar"

    -- Identificadores y Literales (con valores adentro)
    show (TkIdent id)   = "TkIdent(\"" ++ id ++ "\")"
    show (TkNum n)      = "TkNum(" ++ show n ++ ")"
    show TkTrue         = "TkTrue"
    show TkFalse        = "TkFalse"
    
    show (TkCaracter c) = "TkCaracter(" ++ show c ++ ")"
    
    -- Operadores y Separadores
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
