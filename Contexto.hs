module Contexto where

import AST
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (when)

-- Representación interna de los tipos

-- TDesconocido se usa para variables cuyo tipo sólo puede saberse en tiempo de ejecución (collect/read)
data TipoVar = TInt | TBool | TChar | TDesconocido
    deriving (Eq)

instance Show TipoVar where
    show TInt         = "int"
    show TBool        = "bool"
    show TChar        = "char"
    show TDesconocido = "desconocido"

-- Convierte el tipo sintáctico (AST) al tipo semántico usado aquí
convertirTipo :: Type -> TipoVar
convertirTipo TyInt  = TInt
convertirTipo TyBool = TBool
convertirTipo TyChar = TChar

-- Estado del analizador

data Entorno = Entorno {
    tablas          :: [Map.Map String TipoVar],    -- Tabla de símbolos jerárquica (head = alcance más interno)
    errores         :: [String],                    -- Errores de contexto recolectados
    contextoRobot   :: Maybe TipoVar                -- Nothing en el controlador; Just tipo dentro de un comportamiento
}

type Analizador a = State Entorno a

estadoInicial :: Entorno
estadoInicial = Entorno [Map.empty] [] Nothing

-- Manejo de alcances (scopes)

pushScope :: Analizador ()
pushScope = modify (\e -> e { tablas = Map.empty : tablas e })

popScope :: Analizador ()
popScope = modify (\e -> e { tablas = tail (tablas e) })

-- Declara una variable en el alcance actual. Sólo se verifica redeclaración contra ese alcance (no contra los de afuera),
declarar :: String -> TipoVar -> Analizador ()
declarar nombre tipo = do
    entorno <- get
    let (tablaActual : resto) = tablas entorno
    if Map.member nombre tablaActual
        then reportarError $
            "Error de contexto: la variable '" ++ nombre ++ "' ya había sido declarada en este mismo alcance."
        else put entorno { tablas = Map.insert nombre tipo tablaActual : resto }

-- Busca una variable recorriendo las tablas desde el alcance más interno hacia el más externo.
--      devuelve Nothing si no se encuentra en ninguna.
buscarTipo :: String -> Analizador (Maybe TipoVar)
buscarTipo nombre = do
    entorno <- get
    return (buscarEnTablas nombre (tablas entorno))
  where
    buscarEnTablas _ []       = Nothing
    buscarEnTablas n (t : ts) = case Map.lookup n t of
        Just v  -> Just v
        Nothing -> buscarEnTablas n ts

-- Entra al entorno "aislado" de un comportamiento de robot
-- Al terminar, se restaura la pila de tablas y el contexto anteriores
-- Los errores acumulados sí se conservan.
conEntornoDeComportamiento :: TipoVar -> Analizador () -> Analizador ()
conEntornoDeComportamiento tipoRobot analisis = do
    entornoPrevio <- get
    put entornoPrevio { tablas = [Map.empty], contextoRobot = Just tipoRobot }
    analisis
    entornoTrasAnalisis <- get
    put entornoTrasAnalisis {
        tablas        = tablas entornoPrevio,
        contextoRobot = contextoRobot entornoPrevio
    }

reportarError :: String -> Analizador ()
reportarError err = modify (\e -> e { errores = errores e ++ [err] })

-- Punto de entrada

analizarPrograma :: Program -> [String]
analizarPrograma prog =
    let estadoFinal = execState (checkProgram prog) estadoInicial
    in errores estadoFinal

-- Declaraciones y comportamientos

-- Abre el alcance, procesa declaraciones, cierra el alcance
checkProgram :: Program -> Analizador ()
checkProgram (Program decls instr) = do
    pushScope
    mapM_ checkDecl decls
    checkInstr instr
    popScope

-- Revisa secuencialmente las declaraciones para verificar error de duplicados o redeclaracion
checkDecl :: Decl -> Analizador ()
checkDecl (Decl ty ids behaviors) = do
    let tipo = convertirTipo ty
    mapM_ (\ident -> declarar ident tipo) ids
    -- Verifica comportamiento 'default': a lo sumo uno, y debe ir después de todos los demás (excepto activation/deactivation).
    checkOrdenDefault behaviors
    mapM_ (checkBehavior tipo) behaviors

checkOrdenDefault :: [Behavior] -> Analizador ()
checkOrdenDefault behaviors =
    let esDefault (Behavior OnDefault _) = True
        esDefault _                      = False
        esExpr (Behavior (OnExpr _) _)   = True
        esExpr _                         = False
        indicesDefault = [i | (i, b) <- zip [0 :: Int ..] behaviors, esDefault b]
    in case indicesDefault of
        []  -> return ()
        [i] -> when (any esExpr (drop (i + 1) behaviors)) $
                 reportarError
                   "Error de contexto: 'default' debe ir después de todos los demás comportamientos con expresión."
        _   -> reportarError
                 "Error de contexto: un robot no puede tener más de un comportamiento 'default'."

-- Revisa las condiciones e instrucciones del comportamiento asilado
checkBehavior :: TipoVar -> Behavior -> Analizador ()
checkBehavior tipoRobot (Behavior cond instr) =
    conEntornoDeComportamiento tipoRobot $ do
        checkCondition cond
        checkInstr instr

checkCondition :: Condition -> Analizador ()
checkCondition OnActivation   = return ()
checkCondition OnDeactivation = return ()
checkCondition OnDefault      = return ()
checkCondition (OnExpr e)     = do
    t <- tipoExpr e
    case t of
        Just TBool        -> return ()
        Just TDesconocido -> return ()
        Just otro         -> reportarError $
            "Error de tipo: la condición de un comportamiento debe ser de tipo bool, se encontró " ++ show otro ++ "."
        Nothing           -> return () -- el error ya fue reportado dentro de tipoExpr

-- Instrucciones

checkInstr :: Instr -> Analizador ()

checkInstr (Seq i1 i2) = checkInstr i1 >> checkInstr i2

checkInstr (Scope decls instr) = do
    pushScope
    mapM_ checkDecl decls
    checkInstr instr
    popScope

-- Instrucciones exclusivas del controlador
checkInstr (Activate ids)   = checkInstrDeControlador "activate"   (mapM_ checkVarUsada ids)
checkInstr (Advance ids)    = checkInstrDeControlador "advance"    (mapM_ checkVarUsada ids)
checkInstr (Deactivate ids) = checkInstrDeControlador "deactivate" (mapM_ checkVarUsada ids)

checkInstr (If cond thenInstr elseInstr) = do
    t <- tipoExpr cond
    checkEsBool t "la condición de un 'if'"
    checkInstr thenInstr
    maybe (return ()) checkInstr elseInstr

checkInstr (While cond instr) = do
    t <- tipoExpr cond
    checkEsBool t "la condición de un 'while'"
    checkInstr instr

-- Instrucciones exclusivas de un comportamiento de robot
checkInstr (Store e) = checkInstrDeRobot "store" $ \tipoRobot -> do
    t <- tipoExpr e
    case t of
        Just t' | t' /= tipoRobot && t' /= TDesconocido ->
            reportarError $
                "Error de tipo: se intenta almacenar un valor de tipo " ++ show t' ++
                " en un robot de tipo " ++ show tipoRobot ++ "."
        _ -> return ()

checkInstr (Collect mIdent) = checkInstrDeRobot "collect" $ \_ ->
    case mIdent of
        Just ident -> declarar ident TDesconocido
        Nothing    -> return ()

checkInstr (Drop e) = checkInstrDeRobot "drop" $ \_ -> tipoExpr e >> return ()

checkInstr (Move _ mExpr) = checkInstrDeRobot "movimiento" $ \_ ->
    case mExpr of
        Nothing -> return ()
        Just e  -> do
            t <- tipoExpr e
            case t of
                Just TInt         -> return ()
                Just TDesconocido -> return ()
                Just otro         -> reportarError $
                    "Error de tipo: la magnitud de un movimiento debe ser de tipo int, se encontró " ++ show otro ++ "."
                Nothing           -> return ()

checkInstr (Read mIdent) = checkInstrDeRobot "read" $ \_ ->
    case mIdent of
        Just ident -> declarar ident TDesconocido
        Nothing    -> return ()

checkInstr Send = checkInstrDeRobot "send" (const (return ()))

-- Verifica que una instrucción de controlador no se use dentro de un comportamiento
checkInstrDeControlador :: String -> Analizador () -> Analizador ()
checkInstrDeControlador nombre analisis = do
    entorno <- get
    case contextoRobot entorno of
        Just _  -> reportarError $
            "Error de contexto: la instrucción '" ++ nombre ++ "' sólo puede usarse en el controlador principal."
        Nothing -> analisis

-- Verifica que una instrucción de robot no se use en el controlador
checkInstrDeRobot :: String -> (TipoVar -> Analizador ()) -> Analizador ()
checkInstrDeRobot nombre analisis = do
    entorno <- get
    case contextoRobot entorno of
        Nothing        -> reportarError $
            "Error de contexto: la instrucción '" ++ nombre ++ "' sólo puede usarse dentro de un comportamiento de robot."
        Just tipoRobot -> analisis tipoRobot

checkVarUsada :: String -> Analizador ()
checkVarUsada ident = do
    t <- buscarTipo ident
    case t of
        Nothing -> reportarError $
            "Error de contexto: la variable '" ++ ident ++ "' no ha sido declarada."
        Just _  -> return ()

checkEsBool :: Maybe TipoVar -> String -> Analizador ()
checkEsBool (Just TBool) _        = return ()
checkEsBool (Just TDesconocido) _ = return ()
checkEsBool (Just otro) desc      = reportarError $
    "Error de tipo: " ++ desc ++ " debe ser de tipo bool, se encontró " ++ show otro ++ "."
checkEsBool Nothing _             = return ()

-- Análisis de tipos de expresiones

tipoExpr :: Expr -> Analizador (Maybe TipoVar)

tipoExpr (LitInt _)  = return (Just TInt)
tipoExpr (LitBool _) = return (Just TBool)
tipoExpr (LitChar _) = return (Just TChar)

tipoExpr (Var "me") = do
    entorno <- get
    case contextoRobot entorno of
        Nothing -> do
            reportarError "Error de contexto: uso de la palabra reservada 'me' fuera de un comportamiento."
            return Nothing
        Just tipoRobot -> return (Just tipoRobot)

tipoExpr (Var ident) = do
    t <- buscarTipo ident
    case t of
        Nothing -> do
            reportarError $
                "Error de contexto: la variable '" ++ ident ++ "' no ha sido declarada."
            return Nothing
        Just ty -> return (Just ty)

tipoExpr (OpBin op e1 e2) = do
    t1 <- tipoExpr e1
    t2 <- tipoExpr e2
    case (t1, t2) of
        (Just a, Just b) -> tipoOpBin op a b
        _                -> return Nothing -- ya se reportó el error dentro de alguna subexpresión

tipoExpr (OpUn op e) = do
    t <- tipoExpr e
    case t of
        Just a  -> tipoOpUn op a
        Nothing -> return Nothing

-- Reglas de tipo para operadores binarios
tipoOpBin :: Op -> TipoVar -> TipoVar -> Analizador (Maybe TipoVar)
tipoOpBin op a b
    | op `elem` [Add, Sub, Mul, Div, Mod] =
        if compatibles a TInt && compatibles b TInt
            then return (Just TInt)
            else errorOp op a b

    | op `elem` [And, Or] =
        if compatibles a TBool && compatibles b TBool
            then return (Just TBool)
            else errorOp op a b

    | op `elem` [Lt, Le, Gt, Ge] =
        if compatibles a TInt && compatibles b TInt
            then return (Just TBool)
            else errorOp op a b

    | op `elem` [Eq, Neq] =
        if a == TDesconocido || b == TDesconocido || a == b
            then return (Just TBool)
            else errorOp op a b

    | otherwise = return Nothing
  where
    compatibles x y = x == TDesconocido || x == y

    errorOp o x y = do
        reportarError $
            "Error de tipo: el operador '" ++ descOp o ++ "' no admite operandos de tipo " ++
            show x ++ " y " ++ show y ++ "."
        return Nothing

-- Reglas de tipo para operadores unarios
tipoOpUn :: UnaryOp -> TipoVar -> Analizador (Maybe TipoVar)
tipoOpUn Not a
    | a == TBool || a == TDesconocido = return (Just TBool)
    | otherwise = do
        reportarError $ "Error de tipo: el operador '~' requiere un operando de tipo bool, se encontró " ++ show a ++ "."
        return Nothing
tipoOpUn Neg a
    | a == TInt || a == TDesconocido = return (Just TInt)
    | otherwise = do
        reportarError $ "Error de tipo: el operador '-' unario requiere un operando de tipo int, se encontró " ++ show a ++ "."
        return Nothing