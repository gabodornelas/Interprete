module AST where

import Data.List (intercalate)

-- Clase de tipos

class NodoAST a where
    imprimir :: Int -> a -> String 

-- Función auxiliar para generar espacios de indentación
indentar :: Int -> String
indentar n = replicate (n * 4) ' '

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
    | Read (Maybe String)
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

-- Instancia para las Instrucciones. Las comentadas nose usaran en esta etapa
instance NodoAST Instr where

    imprimir nivel (Seq i1 i2) = 
        indentar nivel ++ --"\n" ++             -- en las comillas debe ir SECUENCIACIÓN antes de \n
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
        indentar (nivel + 1) ++ "condición: " ++ imprimir (nivel + 1) cond ++ "\n" ++
        indentar (nivel + 1) ++ "éxito:\n" ++ imprimir (nivel + 2) thenInstr ++
        case elseInstr of
            Just e  -> "\n" ++ indentar (nivel + 1) ++ "fallo:\n" ++ imprimir (nivel + 2) e
            Nothing -> ""

    imprimir nivel (Deactivate ids) =
        indentar nivel ++ "DESACTIVACIÓN\n" ++ 
        indentar (nivel + 1) ++ "var: " ++ intercalate ", " ids

    imprimir nivel (While cond instr) = 
        indentar nivel ++ "ITERACIÓN INDETERMINADA\n" ++ 
        indentar (nivel + 1) ++ "condición: " ++ imprimir (nivel + 1) cond ++ "\n" ++
        indentar (nivel + 1) ++ "éxito:\n" ++ imprimir (nivel + 2) instr

    -- Omitimos por ahora porque aun no esta claro el alcance en el lenguaje BOT para esta etapa
    --imprimir nivel (Scope decls instr) = 
    --    indentar nivel ++ "INCORPORACIÓN ALCANCE\n" ++ 
    --    indentar (nivel + 1) ++ "cuerpo:\n" ++ imprimir (nivel + 2) instr

--    imprimir nivel (Store exp) = 
--        indentar nivel ++ "ALMACENAMIENTO\n" ++ 
--        indentar (nivel + 1) ++ "expr: " ++ imprimir (nivel + 1) exp

--    imprimir nivel (Collect mvar) = 
--        indentar nivel ++ "COLECCIÓN" ++
--        case mvar of
--            Just idVar -> "\n" ++ indentar (nivel + 1) ++ "as: " ++ idVar
--            Nothing    -> ""

--    imprimir nivel (Drop exp) = 
--        indentar nivel ++ "SOLTADO\n" ++ 
--        indentar (nivel + 1) ++ "expr: " ++ imprimir (nivel + 1) exp

--    imprimir nivel (Move dir expOpt) = 
--        indentar nivel ++ "MOVIMIENTO\n" ++ 
--        indentar (nivel + 1) ++ "dir: " ++ mostrarDir dir ++
--        (case expOpt of
--            Just e  -> "\n" ++ indentar (nivel + 1) ++ "cant: " ++ imprimir (nivel + 1) e
--            Nothing -> "")

--    imprimir nivel (Read mvar) = 
--        indentar nivel ++ "ENTRADA" ++
--        case mvar of
--            Just idVar -> "\n" ++ indentar (nivel + 1) ++ "as: " ++ idVar
--            Nothing    -> ""
    
--    imprimir nivel Send = 
--        indentar nivel ++ "SALIDA"

    imprimir nivel _ = ""
        

-- Instancia para las Expresiones
instance NodoAST Expr where
    imprimir nivel (OpBin op e1 e2) = 
        categoriaOp op ++ "\n" ++
        indentar (nivel + 1) ++ "op: '" ++ descOp op ++ "'\n" ++
        indentar (nivel + 1) ++ "izq: " ++ imprimir (nivel + 1) e1 ++ "\n" ++
        indentar (nivel + 1) ++ "der: " ++ imprimir (nivel + 1) e2

    imprimir nivel (OpUn op e) = 
        categoriaOpUn op ++ "\n" ++
        indentar (nivel + 1) ++ "op: '" ++ descOpUn op ++ "'\n" ++
        indentar (nivel + 1) ++ "de: " ++ imprimir (nivel + 1) e

    imprimir nivel expr = valorExpr expr


-- Funciones Auxiliares

-- Para expresiones
valorExpr :: Expr -> String
valorExpr (LitInt n)   = show n
valorExpr (LitBool b)  = if b then "true" else "false"
valorExpr (LitChar c)  = show c
valorExpr (Var s)      = s

-- Para operaciones
categoriaOp :: Op -> String
categoriaOp op 
    | op `elem` [Lt, Le, Gt, Ge, Eq, Neq] = "BIN_RELACIONAL"
    | op `elem` [Add, Sub, Mul, Div, Mod] = "BIN_ARITMÉTICO"
    | op `elem` [And, Or]                 = "BIN_LÓGICO"

-- Para operadores unarios
categoriaOpUn :: UnaryOp -> String
categoriaOpUn Not = "UNARIO_LÓGICO"
categoriaOpUn Neg = "UNARIO_ARITMÉTICO"

-- Para describir el operadores
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

-- Para describir operadores unarios
descOpUn :: UnaryOp -> String
descOpUn Not = "Negación Lógica"
descOpUn Neg = "Inverso Aritmético"

-- Para direcciones
mostrarDir :: Dir -> String
mostrarDir DirLeft  = "Izquierda"
mostrarDir DirRight = "Derecha"
mostrarDir DirUp    = "Arriba"
mostrarDir DirDown  = "Abajo"