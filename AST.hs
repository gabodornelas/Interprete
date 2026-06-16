module AST where

import Data.List (intercalate)

-- ====================================================================
-- 1. CLASE DE TIPOS
-- ====================================================================
class NodoAST a where
    imprimir :: Int -> a -> String 

-- Función auxiliar para generar espacios de indentación
indentar :: Int -> String
indentar n = replicate (n * 2) ' '

-- ====================================================================
-- 2. TIPOS DE DATOS (Estructura Completa: Se almacena todo para el futuro)
-- ====================================================================

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
  | Scope [Decl] Instr           -- Bloque local (create ... execute ... end)
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
  | BinOp Op Expr Expr
  | UnOp UnaryOp Expr

data Op = Add | Sub | Mul | Div | Mod | And | Or | Lt | Le | Gt | Ge | Eq | Neq
data UnaryOp = Not | Neg

-- ====================================================================
-- 3. INSTANCIAS (Filtrado de Salida: Solo se imprime lo requerido)
-- ====================================================================

-- Instancia para el Programa Principal
instance NodoAST Program where
    imprimir nivel (Program decls instrs) = 
        -- Cumplimos el PDF: Las declaraciones de robots y sus acciones existen en el data,
        -- pero NO se reflejan en la salida generada. Evaluamos directo las instrucciones.
        imprimir nivel instrs

-- Instancia para las Instrucciones
instance NodoAST Instr where
    imprimir nivel (Seq i1 i2) = 
        indentar nivel ++ "SECUENCIACION\n" ++ 
        imprimir nivel i1 ++ "\n" ++ 
        imprimir nivel i2
        
    imprimir nivel (Activate ids) = 
        indentar nivel ++ "ACTIVACION\n" ++ 
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
        indentar nivel ++ "DESACTIVACION\n" ++ 
        indentar (nivel + 1) ++ "var: " ++ intercalate ", " ids

    imprimir nivel (While cond instr) = 
        indentar nivel ++ "ITERACION INDETERMINADA\n" ++ 
        indentar (nivel + 1) ++ "guardia: " ++ imprimir (nivel + 1) cond ++ "\n" ++
        indentar (nivel + 1) ++ "exito:\n" ++ imprimir (nivel + 2) instr

    imprimir nivel (Scope decls instr) = 
        -- El bloque local genera la etiqueta, pero omitimos recorrer sus 'decls' locales
        indentar nivel ++ "INCORPORACION ALCANCE\n" ++ 
        indentar (nivel + 1) ++ "cuerpo:\n" ++ imprimir (nivel + 2) instr

    imprimir nivel (Store exp) = 
        indentar nivel ++ "ALMACENAMIENTO\n" ++ 
        indentar (nivel + 1) ++ "expresion: " ++ imprimir (nivel + 1) exp

    imprimir nivel (Collect mvar) = 
        indentar nivel ++ "COLECCION\n" ++
        case mvar of
            Just idVar -> indentar (nivel + 1) ++ "as: " ++ idVar
            Nothing    -> ""

    imprimir nivel (Drop exp) = 
        indentar nivel ++ "SOLTADO\n" ++ 
        indentar (nivel + 1) ++ "expresion: " ++ imprimir (nivel + 1) exp

    imprimir nivel (Move dir expOpt) = 
        indentar nivel ++ "MOVIMIENTO\n" ++ 
        indentar (nivel + 1) ++ "direccion: " ++ mostrarDir dir ++
        (case expOpt of
            Just e  -> "\n" ++ indentar (nivel + 1) ++ "cantidad: " ++ imprimir (nivel + 1) e ++ " unidades"
            Nothing -> "")

    imprimir nivel (Read mvar) = 
        indentar nivel ++ "ENTRADA\n" ++
        case mvar of
            Just idVar -> indentar (nivel + 1) ++ "as: " ++ idVar
            Nothing    -> ""

    imprimir nivel Receive = 
        indentar nivel ++ "ENTRADA"
    
    imprimir nivel Send = 
        indentar nivel ++ "SALIDA"
        
    imprimir nivel _ = indentar nivel ++ "OTRA_INSTRUCCION"


-- Instancia para las Expresiones
instance NodoAST Expr where
    imprimir nivel (BinOp op e1 e2) = 
        categoriaOp op ++ "\n" ++
        indentar (nivel + 1) ++ "operacion: '" ++ descOp op ++ "'\n" ++
        indentar (nivel + 1) ++ "operador izquierdo: " ++ valorExpr e1 ++ "\n" ++
        indentar (nivel + 1) ++ "operador derecho: " ++ valorExpr e2

    imprimir nivel (UnOp op e) = 
        categoriaUnOp op ++ "\n" ++
        indentar (nivel + 1) ++ "operacion: '" ++ descUnOp op ++ "'\n" ++
        indentar (nivel + 1) ++ "operador: " ++ valorExpr e

    imprimir nivel expr = valorExpr expr


-- ====================================================================
-- 4. FUNCIONES AUXILIARES
-- ====================================================================

valorExpr :: Expr -> String
valorExpr (LitInt n)   = show n
valorExpr (LitBool b)  = if b then "true" else "false"
valorExpr (LitChar c)  = show c
valorExpr (Var s)      = s
valorExpr _            = "EXPRESION_COMPLEJA"

categoriaOp :: Op -> String
categoriaOp op 
    | op `elem` [Lt, Le, Gt, Ge, Eq, Neq] = "BIN_RELACIONAL"
    | op `elem` [Add, Sub, Mul, Div, Mod] = "BIN_ARITMETICO"
    | otherwise                           = "BIN_LOGICO"

categoriaUnOp :: UnaryOp -> String
categoriaUnOp Not = "UNARIO_LOGICO"
categoriaUnOp Neg = "UNARIO_ARITMETICO"

descOp :: Op -> String
descOp Add = "Suma"
descOp Sub = "Resta"
descOp Mul = "Multiplicacion"
descOp Div = "Division Entera"
descOp Mod = "Modulo"
descOp And = "Conjuncion"
descOp Or  = "Disyuncion"
descOp Gt  = "Mayor que"
descOp Ge  = "Mayor o igual que"
descOp Lt  = "Menor que"
descOp Le  = "Menor o igual que"
descOp Eq  = "Igual a"
descOp Neq = "Distinto de"

descUnOp :: UnaryOp -> String
descUnOp Not = "Negacion Logica"
descUnOp Neg = "Inverso Aritmetico"

mostrarDir :: Dir -> String
mostrarDir DirLeft  = "left"
mostrarDir DirRight = "right"
mostrarDir DirUp    = "up"
mostrarDir DirDown  = "down"