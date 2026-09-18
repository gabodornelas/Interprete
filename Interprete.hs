module Interprete (ejecutarPrograma) where

import AST
import Tokens ()
import qualified Data.Map as Map
import Data.Char (isDigit)
import Control.Monad.State
import Control.Exception (try, IOException)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)


-- Representación de valores en tiempo de ejecución

-- Valor concreto que puede tomar una expresión, un robot o una celda de la matriz.
data Valor = VInt Int | VBool Bool | VChar Char

-- Tipo semántico de un valor.
data TipoVal = TVInt | TVBool | TVChar deriving (Eq)

instance Show TipoVal where
    show TVInt  = "int"
    show TVBool = "bool"
    show TVChar = "char"

tipoDeValor :: Valor -> TipoVal
tipoDeValor (VInt _)  = TVInt
tipoDeValor (VBool _) = TVBool
tipoDeValor (VChar _) = TVChar

tipoDeclarado :: Type -> TipoVal
tipoDeclarado TyInt  = TVInt
tipoDeclarado TyBool = TVBool
tipoDeclarado TyChar = TVChar

-- Representación textual de un valor tal como BOT lo imprime (instrucción send): sin comillas, sin espacios ni saltos de línea añadidos.
mostrarValor :: Valor -> String
mostrarValor (VInt n)  = show n
mostrarValor (VBool b) = if b then "true" else "false"
mostrarValor (VChar c) = [c]

-- Estado de un robot

data EstadoRobot = EstadoRobot
    { erTipo    :: TipoVal        -- Tipo asociado al robot
    , erValor   :: Maybe Valor    -- Valor actualmente almacenado (Nothing si nunca se ha guardado nada)
    , erActivo  :: Bool           -- Si el robot está actualmente activo
    , erPos     :: (Int, Int)     -- Posición actual del robot en la matriz
    , erComport :: [Behavior]     -- Comportamientos del robot (los declarados)
    }

-- Entorno de identificadores

data Entrada = ERobot EstadoRobot | EVar Valor

type Marco = Map.Map String Entrada

-- Estado global del intérprete

data Estado = Estado
    { pila        :: [Marco]                     -- Pila de marcos (alcances). El tope es el más interno.
    , matriz      :: Map.Map (Int, Int) Valor    -- La matriz de celdas
    , robotActual :: Maybe String                -- Nombre del robot cuyo comportamiento se ejecuta (Nothing = controlador)
    }

type Interprete a = StateT Estado IO a

estadoInicial :: Estado
estadoInicial = Estado { pila = [Map.empty], matriz = Map.empty, robotActual = Nothing }

-- Manejo de errores dinámicos

-- Reporta un error de ejecución y aborta el programa completo.
errorEjecucion :: String -> Interprete a
errorEjecucion msg = liftIO $ do
    putStrLn $ "Error de ejecución: " ++ msg
    exitFailure

-- Manejo de la pila de marcos (alcances)

pushMarco :: Interprete ()
pushMarco = modify (\e -> e { pila = Map.empty : pila e })

popMarco :: Interprete ()
popMarco = modify (\e -> e { pila = tail (pila e) })

-- Declara/actualiza una entrada en el marco más interno (tope de la pila).
declararEnTope :: String -> Entrada -> Interprete ()
declararEnTope nombre entrada = modify (\e ->
    let (m : ms) = pila e
    in e { pila = Map.insert nombre entrada m : ms })

-- Busca una entrada recorriendo los marcos desde el más interno al más externo.
buscarEntrada :: String -> Interprete (Maybe Entrada)
buscarEntrada nombre = gets (buscarEnMarcos . pila)
  where
    buscarEnMarcos []       = Nothing
    buscarEnMarcos (m : ms) = case Map.lookup nombre m of
        Just v  -> Just v
        Nothing -> buscarEnMarcos ms

-- Actualiza (sobreescribe) una entrada ya existente en el marco donde se encuentra.
actualizarEntrada :: String -> Entrada -> Interprete ()
actualizarEntrada nombre entrada = modify (\e -> e { pila = actualizar (pila e) })
  where
    actualizar []       = []
    actualizar (m : ms)
        | Map.member nombre m = Map.insert nombre entrada m : ms
        | otherwise            = m : actualizar ms

-- Obtiene el estado de un robot por nombre.
obtenerRobot :: String -> Interprete EstadoRobot
obtenerRobot nombre = do
    mEntrada <- buscarEntrada nombre
    case mEntrada of
        Just (ERobot er) -> return er
        _                -> errorEjecucion $ "referencia interna inválida al robot '" ++ nombre ++ "'."

actualizarRobot :: String -> EstadoRobot -> Interprete ()
actualizarRobot nombre er = actualizarEntrada nombre (ERobot er)

-- Nombre del robot en cuyo comportamiento nos encontramos actualmente.
robotEnContexto :: Interprete String
robotEnContexto = do
    e <- get
    case robotActual e of
        Just nombre -> return nombre
        Nothing     -> errorEjecucion "uso de una instrucción de robot fuera de un comportamiento."

-- Declaración de robots (create ... bot ...)
declararDecl :: Decl -> Interprete ()
declararDecl (Decl ty ids comportamientos) = do
    let tipo = tipoDeclarado ty
        nuevo = EstadoRobot { erTipo = tipo, erValor = Nothing, erActivo = False
                             , erPos = (0, 0), erComport = comportamientos }
    mapM_ (\ident -> declararEnTope ident (ERobot nuevo)) ids

-- Evaluación de expresiones

evaluar :: Expr -> Interprete Valor

evaluar (LitInt n)  = return (VInt n)
evaluar (LitBool b) = return (VBool b)
evaluar (LitChar c) = return (VChar c)

-- La variable especial "me" siempre refiere al valor asociado al robot actual.
evaluar (Var "me") = do
    nombre <- robotEnContexto
    er <- obtenerRobot nombre
    case erValor er of
        Just v  -> return v
        Nothing -> errorEjecucion $
            "se intentó usar el valor del robot '" ++ nombre ++ "' pero no ha almacenado nada aún."

evaluar (Var ident) = do
    mEntrada <- buscarEntrada ident
    case mEntrada of
        Just (EVar v) -> return v
        Just (ERobot er) -> case erValor er of
            Just v  -> return v
            Nothing -> errorEjecucion $
                "se intentó usar el valor del robot '" ++ ident ++ "' pero no ha almacenado nada aún."
        Nothing -> errorEjecucion $
            "la variable '" ++ ident ++ "' no está declarada."

evaluar (OpUn Neg e) = do
    v <- evaluar e
    case v of
        VInt n -> return (VInt (negate n))
        _      -> errorEjecucion "operador de inverso aritmético ('-') aplicado a un valor que no es entero."

evaluar (OpUn Not e) = do
    v <- evaluar e
    case v of
        VBool b -> return (VBool (not b))
        _       -> errorEjecucion "operador de negación lógica ('~') aplicado a un valor que no es booleano."

evaluar (OpBin op e1 e2) = do
    v1 <- evaluar e1
    v2 <- evaluar e2
    evaluarOpBin op v1 v2

-- Aplica un operador binario a dos valores ya evaluados, verificando los errores dinámicos correspondientes
--      En especial, la división por cero.
evaluarOpBin :: Op -> Valor -> Valor -> Interprete Valor

evaluarOpBin Add (VInt a) (VInt b) = return (VInt (a + b))
evaluarOpBin Sub (VInt a) (VInt b) = return (VInt (a - b))
evaluarOpBin Mul (VInt a) (VInt b) = return (VInt (a * b))

evaluarOpBin Div (VInt _) (VInt 0) = errorEjecucion "se intentó dividir entre cero."
evaluarOpBin Div (VInt a) (VInt b) = return (VInt (a `quot` b))

evaluarOpBin Mod (VInt _) (VInt 0) = errorEjecucion "se intentó calcular un módulo entre cero."
evaluarOpBin Mod (VInt a) (VInt b) = return (VInt (a `rem` b))

evaluarOpBin And (VBool a) (VBool b) = return (VBool (a && b))
evaluarOpBin Or  (VBool a) (VBool b) = return (VBool (a || b))

evaluarOpBin Lt (VInt a) (VInt b) = return (VBool (a < b))
evaluarOpBin Le (VInt a) (VInt b) = return (VBool (a <= b))
evaluarOpBin Gt (VInt a) (VInt b) = return (VBool (a > b))
evaluarOpBin Ge (VInt a) (VInt b) = return (VBool (a >= b))

evaluarOpBin Eq (VInt a)  (VInt b)  = return (VBool (a == b))
evaluarOpBin Eq (VBool a) (VBool b) = return (VBool (a == b))
evaluarOpBin Eq (VChar a) (VChar b) = return (VBool (a == b))

evaluarOpBin Neq (VInt a)  (VInt b)  = return (VBool (a /= b))
evaluarOpBin Neq (VBool a) (VBool b) = return (VBool (a /= b))
evaluarOpBin Neq (VChar a) (VChar b) = return (VBool (a /= b))

-- No debería ocurrir si el análisis de contexto fue exitoso, pero se deja el caso de variables de tipo desconocido.
evaluarOpBin op v1 v2 = errorEjecucion $
    "el operador '" ++ show op ++ "' no puede aplicarse a valores de tipo " ++
    show (tipoDeValor v1) ++ " y " ++ show (tipoDeValor v2) ++ "."

instance Show Op where
    show Add = "+"; show Sub = "-"; show Mul = "*"; show Div = "/"; show Mod = "%"
    show And = "/\\"; show Or = "\\/"; show Lt = "<"; show Le = "<="
    show Gt = ">"; show Ge = ">="; show Eq = "="; show Neq = "/="

-- Direcciones de movimiento

moverPosicion :: Dir -> Int -> (Int, Int) -> (Int, Int)
moverPosicion DirLeft  n (x, y) = (x - n, y)
moverPosicion DirRight n (x, y) = (x + n, y)
moverPosicion DirUp    n (x, y) = (x, y + n)
moverPosicion DirDown  n (x, y) = (x, y - n)

-- Lectura de entrada estándar (instrucción read)

-- Elimina un posible '\r' final (del estilo Windows).
quitarCR :: String -> String
quitarCR s = case reverse s of
    ('\r' : rest) -> reverse rest
    _             -> s

-- Dada la cadena que escribió el usuario, calcula todas las interpretaciones válidas como literal de BOT (puede haber más de una:
--      ejemplo, "7" es válido tanto como entero como como carácter). El orden de la lista determina la preferencia cuando el tipo
--      no está forzado por el tipo del robot (se prefiere int, luego char).
candidatosEntrada :: String -> [Valor]
candidatosEntrada s
    | s == "true"  = [VBool True]
    | s == "false" = [VBool False]
    | otherwise    = candidatosInt ++ candidatosChar
  where
    esDigitos ds = not (null ds) && all isDigit ds
    esEntero = case s of
        ('-' : ds) -> esDigitos ds
        ds         -> esDigitos ds
    candidatosInt  = [VInt (read s) | esEntero]
    candidatosChar = [VChar (head s) | length s == 1]

-- Pide una línea al usuario y la interpreta según el tipo esperado (si se conoce, es decir, si no está presente el "as")
--      Reporta "Lectura inadecuada" si la entrada no es consistente con lo esperado.
leerValor :: Maybe TipoVal -> Interprete Valor
leerValor tipoEsperado = do
    resultado <- liftIO (try getLine :: IO (Either IOException String))
    case resultado of
        Left _     -> errorEjecucion "no se pudo leer una entrada del usuario (fin de archivo inesperado)."
        Right cruda -> do
            let entrada = quitarCR cruda
                candidatos = candidatosEntrada entrada
            case tipoEsperado of
                Nothing -> case candidatos of
                    (v : _) -> return v
                    []      -> errorEjecucion $
                        "la entrada \"" ++ entrada ++ "\" no corresponde a ningún tipo válido de BOT (int, bool o char)."
                Just t -> case filter ((== t) . tipoDeValor) candidatos of
                    (v : _) -> return v
                    []      -> errorEjecucion $
                        "la entrada \"" ++ entrada ++ "\" es inconsistente con el tipo " ++ show t ++ " esperado."

-- Ejecución de instrucciones

correr :: Instr -> Interprete ()

correr (Seq i1 i2) = correr i1 >> correr i2

-- Instrucciones exclusivas del controlador
correr (Activate ids)   = mapM_ activarUno ids
correr (Advance ids)    = mapM_ avanzarUno ids
correr (Deactivate ids) = mapM_ desactivarUno ids

correr (If cond i1 mi2) = do
    v <- evaluar cond
    case v of
        VBool True  -> correr i1
        VBool False -> maybe (return ()) correr mi2
        _           -> errorEjecucion "la condición de un 'if' no evaluó a un valor booleano."

correr w@(While cond instr) = do
    v <- evaluar cond
    case v of
        VBool True  -> correr instr >> correr w
        VBool False -> return ()
        _           -> errorEjecucion "la condición de un 'while' no evaluó a un valor booleano."

-- Incorporación de alcance: declara nuevos robots visibles únicamente durante la ejecución de la instrucción interna;
--      al terminar, el marco se descarta.
correr (Scope decls instr) = do
    pushMarco
    mapM_ declararDecl decls
    correr instr
    popMarco

-- Instrucciones exclusivas de un comportamiento de robot
correr (Store e) = do
    nombre <- robotEnContexto
    v <- evaluar e
    er <- obtenerRobot nombre
    if tipoDeValor v == erTipo er
        then actualizarRobot nombre er { erValor = Just v }
        else errorEjecucion $
            "almacenamiento inadecuado: se intentó guardar un valor de tipo " ++
            show (tipoDeValor v) ++ " en el robot '" ++ nombre ++ "', de tipo " ++ show (erTipo er) ++ "."

correr (Collect mIdent) = do
    nombre <- robotEnContexto
    er <- obtenerRobot nombre
    m <- gets matriz
    case Map.lookup (erPos er) m of
        Nothing -> errorEjecucion $
            "colección inadecuada: la posición " ++ show (erPos er) ++ " de la matriz está vacía."
        Just v -> case mIdent of
            Just ident -> declararEnTope ident (EVar v)
            Nothing ->
                if tipoDeValor v == erTipo er
                    then actualizarRobot nombre er { erValor = Just v }
                    else errorEjecucion $
                        "colección inadecuada: el valor recogido es de tipo " ++ show (tipoDeValor v) ++
                        ", pero el robot '" ++ nombre ++ "' es de tipo " ++ show (erTipo er) ++ "."

correr (Drop e) = do
    nombre <- robotEnContexto
    v <- evaluar e
    er <- obtenerRobot nombre
    if tipoDeValor v == erTipo er
        then modify (\est -> est { matriz = Map.insert (erPos er) v (matriz est) })
        else errorEjecucion $
            "soltado inadecuado: se intentó soltar un valor de tipo " ++ show (tipoDeValor v) ++
            " con el robot '" ++ nombre ++ "', de tipo " ++ show (erTipo er) ++ "."

correr (Move dir mExpr) = do
    nombre <- robotEnContexto
    er <- obtenerRobot nombre
    magnitud <- case mExpr of
        Nothing -> return 1
        Just e  -> do
            v <- evaluar e
            case v of
                VInt n | n >= 0    -> return n
                       | otherwise -> errorEjecucion "la magnitud de un movimiento debe ser no negativa."
                _ -> errorEjecucion "la magnitud de un movimiento debe ser de tipo int."
    actualizarRobot nombre er { erPos = moverPosicion dir magnitud (erPos er) }

correr (Read mIdent) = do
    nombre <- robotEnContexto
    er <- obtenerRobot nombre
    case mIdent of
        Just ident -> do
            v <- leerValor Nothing
            declararEnTope ident (EVar v)
        Nothing -> do
            v <- leerValor (Just (erTipo er))
            actualizarRobot nombre er { erValor = Just v }

correr Send = do
    nombre <- robotEnContexto
    er <- obtenerRobot nombre
    case erValor er of
        Just v  -> liftIO (putStr (mostrarValor v) >> hFlush stdout)
        Nothing -> errorEjecucion $
            "se intentó enviar (send) el valor del robot '" ++ nombre ++ "' pero no ha almacenado nada aún."

-- Instrucciones de controlador: activar, avanzar, desactivar

esActivation :: Behavior -> Bool
esActivation (Behavior OnActivation _) = True
esActivation _                         = False

esDeactivation :: Behavior -> Bool
esDeactivation (Behavior OnDeactivation _) = True
esDeactivation _                           = False

buscarInstr :: (Behavior -> Bool) -> [Behavior] -> Maybe Instr
buscarInstr p bs = case filter p bs of
    (Behavior _ instr : _) -> Just instr
    []                     -> Nothing

-- Ejecuta una instrucción de comportamiento dentro del contexto de un robot: abre un nuevo marco (para las variables
--      locales del comportamiento, p.ej. las declaradas con "collect as x" o "read as x") y fija el robot actual.
ejecutarComportamiento :: String -> Instr -> Interprete ()
ejecutarComportamiento nombre instr = do
    anterior <- gets robotActual
    modify (\e -> e { robotActual = Just nombre })
    pushMarco
    correr instr
    popMarco
    modify (\e -> e { robotActual = anterior })

activarUno :: String -> Interprete ()
activarUno nombre = do
    er <- obtenerRobot nombre
    if erActivo er
        then errorEjecucion $
            "activación ilegal: el robot '" ++ nombre ++ "' ya se encuentra activo."
        else do
            actualizarRobot nombre er { erActivo = True }
            case buscarInstr esActivation (erComport er) of
                Just instr -> ejecutarComportamiento nombre instr
                Nothing    -> return ()

desactivarUno :: String -> Interprete ()
desactivarUno nombre = do
    er <- obtenerRobot nombre
    if not (erActivo er)
        then errorEjecucion $
            "desactivación ilegal: el robot '" ++ nombre ++ "' no se encuentra activo."
        else do
            actualizarRobot nombre er { erActivo = False }
            case buscarInstr esDeactivation (erComport er) of
                Just instr -> ejecutarComportamiento nombre instr
                Nothing    -> return ()

avanzarUno :: String -> Interprete ()
avanzarUno nombre = do
    er <- obtenerRobot nombre
    if not (erActivo er)
        then errorEjecucion $
            "no se puede avanzar el robot '" ++ nombre ++ "' porque no está activo (o no ha sido creado)."
        else do
            anterior <- gets robotActual
            modify (\e -> e { robotActual = Just nombre })
            pushMarco
            resultado <- buscarComportamientoAvance (erComport er)
            case resultado of
                Just instr -> correr instr
                Nothing    -> do
                    popMarco
                    modify (\e -> e { robotActual = anterior })
                    errorEjecucion $
                        "comportamiento inexistente: ningún comportamiento del robot '" ++ nombre ++
                        "' es aplicable al avanzarlo."
            popMarco
            modify (\e -> e { robotActual = anterior })

-- Recorre los comportamientos (ignorando activation/deactivation, que nunca aplican al avanzar) y devuelve la instrucción
--      del primero cuya condición se satisfaga; 'default' siempre se satisface como última opción.
buscarComportamientoAvance :: [Behavior] -> Interprete (Maybe Instr)
buscarComportamientoAvance [] = return Nothing
buscarComportamientoAvance (Behavior OnActivation _ : bs)   = buscarComportamientoAvance bs
buscarComportamientoAvance (Behavior OnDeactivation _ : bs) = buscarComportamientoAvance bs
buscarComportamientoAvance (Behavior OnDefault instr : _)   = return (Just instr)
buscarComportamientoAvance (Behavior (OnExpr cond) instr : bs) = do
    v <- evaluar cond
    case v of
        VBool True  -> return (Just instr)
        VBool False -> buscarComportamientoAvance bs
        _           -> errorEjecucion "la condición de un comportamiento no evaluó a un valor booleano."

-- Punto de entrada
-- Ejecuta un programa BOT ya validado por el análisis de contexto.
ejecutarPrograma :: Program -> IO ()
ejecutarPrograma (Program decls instr) =
    evalStateT ejecucion estadoInicial
  where
    ejecucion = do
        mapM_ declararDecl decls
        correr instr
