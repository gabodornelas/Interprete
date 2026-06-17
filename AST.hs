module AST where

import Data.List (intercalate)

-- Clase de tipos

class NodoAST a where
    imprimir :: Int -> a -> String 

-- Función auxiliar para generar espacios de indentación
indentar :: Int -> String
indentar n = replicate (n * 2) ' '

-- Tipos de datos (Estructura que será de ayuda para siguientes etapas)

-- El programa guarda las declaraciones en el árbol, aunque no se impriman
data Program = Program [Decl] Instr

data Decl = Decl Type [String] [Behavior]
data Type = TyInt | TyBool | TyChar
data Behavior = Behavior Condition Instr
data Condition = OnActivation | OnDeactivation | OnDefault | OnExpr Expr
data Dir = DirLeft | DirRight | DirUp | DirDown

data Instr 
  = Seq Instr Instr
  | Activate [String]
  | Advance [String]
  | Deactivate [String]
  | If Expr Instr (Maybe Instr)
  | While Expr Instr
  | Scope [Decl] Instr           -- El alcance sera importante en futuras entregas
  | Store Expr
  | Collect (Maybe String)
  | Drop Expr
  | Move Dir (Maybe Expr)
  | Read (Maybe String)          -- read [as id]
  | Receive                      -- Para compatibilidad de la entrada
  | Send

data Expr
  = LitInt Int
  | LitBool Bool
  | LitChar Char
  | Var String
  | OpBin Op Expr Expr
  | OpUn UnaryOp Expr
                                            -- Usamos los nombres en ingles para facilidad de abreviacion
data Op = Add | Sub | Mul | Div | Mod | And | Or | Lt | Le | Gt | Ge | Eq | Neq deriving (Eq)
data UnaryOp = Not | Neg

-- Instancias

-- Instancia para el Programa Principal
instance NodoAST Program where
    imprimir nivel (Program decls instrs) = -- por ahora no imprimimos declaraciones
        imprimir nivel instrs

-- Instancia para las Instrucciones
instance NodoAST Instr where
--OJO
    imprimir nivel (Seq i1 i2) = 
        indentar nivel ++ "SECUENCIACIÓN\n" ++ 
        imprimir nivel i1 ++ "\n" ++ 
        imprimir nivel i2
        
    imprimir nivel (Activate ids) = 
        indentar nivel ++ "ACTIVACIÓN\n" ++ 
        indentar (nivel + 1) ++ "var: " ++ intercalate ", " ids
        
    imprimir nivel (Advance ids) = 
        indentar nivel ++ "AVANCE\n" ++ 
        indentar (nivel + 1) ++ "var: " ++ intercalate ", " ids
        
    imprimir nivel (If cond thenInstr elseInstr) = 
        indentar nivel ++ "CONDICIONAL\n" ++ 
        indentar (nivel + 1) ++ "guardia: " ++ imprimir (nivel + 1) cond ++ "\n" ++
        indentar (nivel + 1) ++ "exito:\n" ++ imprimir (nivel + 2) thenInstr ++
        case elseInstr of
            Just e  -> "\n" ++ indentar (nivel + 1) ++ "fallo:\n" ++ imprimir (nivel + 2) e
            Nothing -> ""

    imprimir nivel (Deactivate ids) =
        indentar nivel ++ "DESACTIVACIÓN\n" ++ 
        indentar (nivel + 1) ++ "var: " ++ intercalate ", " ids

    imprimir nivel (While cond instr) = 
        indentar nivel ++ "ITERACIÓN INDETERMINADA\n" ++ 
        indentar (nivel + 1) ++ "guardia: " ++ imprimir (nivel + 1) cond ++ "\n" ++
        indentar (nivel + 1) ++ "exito:\n" ++ imprimir (nivel + 2) instr

    -- Omitimos por ahora porque aun no esta claro el alcance en el lenguaje BOT para esta etapa
    --imprimir nivel (Scope decls instr) = 
    --    indentar nivel ++ "INCORPORACIÓN ALCANCE\n" ++ 
    --    indentar (nivel + 1) ++ "cuerpo:\n" ++ imprimir (nivel + 2) instr

--OJO
    imprimir nivel (Store exp) = 
        indentar nivel ++ "ALMACENAMIENTO\n" ++ 
        indentar (nivel + 1) ++ "expresion: " ++ imprimir (nivel + 1) exp
--OJO
    imprimir nivel (Collect mvar) = 
        indentar nivel ++ "COLECCIÓN\n" ++
        case mvar of
            Just idVar -> indentar (nivel + 1) ++ "as: " ++ idVar
            Nothing    -> ""
--OJO
    imprimir nivel (Drop exp) = 
        indentar nivel ++ "SOLTADO\n" ++ 
        indentar (nivel + 1) ++ "expresion: " ++ imprimir (nivel + 1) exp
--OJO
    imprimir nivel (Move dir expOpt) = 
        indentar nivel ++ "MOVIMIENTO\n" ++ 
        indentar (nivel + 1) ++ "direccion: " ++ mostrarDir dir ++
        (case expOpt of
            Just e  -> "\n" ++ indentar (nivel + 1) ++ "cantidad: " ++ imprimir (nivel + 1) e ++ " unidades"
            Nothing -> "")
--OJO
    imprimir nivel (Read mvar) = 
        indentar nivel ++ "ENTRADA\n" ++
        case mvar of
            Just idVar -> indentar (nivel + 1) ++ "as: " ++ idVar
            Nothing    -> ""
--OJO
    imprimir nivel Receive = 
        indentar nivel ++ "ENTRADA"
--OJO    
    imprimir nivel Send = 
        indentar nivel ++ "SALIDA"
        

-- Instancia para las Expresiones
instance NodoAST Expr where
    imprimir nivel (OpBin op e1 e2) = 
        categoriaOp op ++ "\n" ++
        indentar (nivel + 1) ++ "operación: '" ++ descOp op ++ "'\n" ++
        indentar (nivel + 1) ++ "operador izquierdo: " ++ valorExpr e1 ++ "\n" ++
        indentar (nivel + 1) ++ "operador derecho: " ++ valorExpr e2

    imprimir nivel (OpUn op e) = 
        categoriaOpUn op ++ "\n" ++
        indentar (nivel + 1) ++ "operación: '" ++ descOpUn op ++ "'\n" ++
        indentar (nivel + 1) ++ "operador: " ++ valorExpr e

    imprimir nivel expr = valorExpr expr


-- Funciones Auxiliares

valorExpr :: Expr -> String
valorExpr (LitInt n)   = show n
valorExpr (LitBool b)  = if b then "true" else "false"
valorExpr (LitChar c)  = show c
valorExpr (Var s)      = s
valorExpr _            = "EXPRESIÓN_COMPLEJA"

categoriaOp :: Op -> String
categoriaOp op 
    | op `elem` [Lt, Le, Gt, Ge, Eq, Neq] = "BIN_RELACIONAL"
    | op `elem` [Add, Sub, Mul, Div, Mod] = "BIN_ARITMÉTICO"
    | otherwise                           = "BIN_LÓGICO"

categoriaOpUn :: UnaryOp -> String
categoriaOpUn Not = "UNARIO_LÓGICO"
categoriaOpUn Neg = "UNARIO_ARITMÉTICO"

descOp :: Op -> String
descOp Add = "Suma"
descOp Sub = "Resta"
descOp Mul = "Multiplicación"
descOp Div = "División Entera"
descOp Mod = "Módulo"
descOp And = "Conjunción"
descOp Or  = "Disyunción"
descOp Gt  = "Mayor que"
descOp Ge  = "Mayor o igual que"
descOp Lt  = "Menor que"
descOp Le  = "Menor o igual que"
descOp Eq  = "Igual a"
descOp Neq = "Desigual a"

descOpUn :: UnaryOp -> String
descOpUn Not = "Negación Lógica"
descOpUn Neg = "Inverso Aritmético"

mostrarDir :: Dir -> String
mostrarDir DirLeft  = "Izquierda"
mostrarDir DirRight = "Derecha"
mostrarDir DirUp    = "Arriba"
mostrarDir DirDown  = "Abajo"