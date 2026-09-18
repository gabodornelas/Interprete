module Main where

import System.Environment (getArgs)                  -- Para los argumentos que trae el ejecutable (el archivo)
import System.Exit (exitFailure, exitSuccess)        -- Para terminar el programa
import Data.List (partition)                         -- Importamos la herramienta para dividir listas
import Reglas (alexScanTokens)                       -- El analizador
import Tokens                                        -- Los Tokens que definimos
import Sintaxis (sintBot)                            -- La gramática definida
import Contexto (analizarPrograma)                   -- Para analizar el contexto
import System.IO (hSetEncoding, stdin, stdout, utf8) -- Para forzar UTF-8 en entrada y salida
import Interprete (ejecutarPrograma)                 -- El intérprete

-- Función que dice si un Token es de clase TkError o no
esError :: Token -> Bool
esError (Token _ _ (TkError _)) = True
esError _                       = False

-- Imprime los errores léxicos con el formato indicado
printErrorLex :: Token -> IO ()
printErrorLex (Token fila col (TkError c)) =
    putStrLn $ "Error Léxico: Caracter inesperado \"" ++ [c] ++ "\" en la fila " ++ show fila ++ ", columna " ++ show col ++ "."
printErrorLex _ = return ()

-- Función para imprimir el error sintáctico
printErrorSint :: [Token] -> IO ()
printErrorSint [] = putStrLn "Error sintáctico: fin de archivo inesperado."
printErrorSint (Token f c cls : _) =
    putStrLn $ "Error sintáctico en fila " ++ show f ++ ", columna " ++ show c ++ " cerca del token " ++ show cls

main :: IO ()
main = do
    -- Forzamos codificación UTF-8 en entrada y salida
    hSetEncoding stdin  utf8
    hSetEncoding stdout utf8

    args <- getArgs

    case args of
        [archivo] -> do -- Exactamente un archivo
            contenidoOriginal <- readFile archivo
            let contenido = map (\c -> if c == '\t' then ' ' else c) contenidoOriginal

            -- Generamos la lista completa (mezclada con válidos y errores)
            let tokens = alexScanTokens contenido

            -- Dividimos la lista en dos. 'partition' recibe la condición (esError) y la lista original
            let (errores, validos) = partition esError tokens

            -- Verificamos si la lista de errores léxicos no está vacía
            if not (null errores)
                then do
                -- Si no está vacía, hay errores léxicos. Los imprimimos y salimos.
                    mapM_ printErrorLex errores
                    exitFailure
                else do
                    -- Si está vacía, no hay errores, analizamos la sintaxis
                    case sintBot validos of
                        -- Si hubo un error sintáctico, recibimos el token más cercano al conflicto
                        Left tokensError -> do
                            printErrorSint tokensError
                            exitFailure

                        -- Si la sintaxis fue correcta, analizamos el contexto
                        Right ast -> do
                            -- Análisis de Contexto
                            let erroresContexto = analizarPrograma ast
                            -- Si no hay errores de contexto, ejecutamos
                            if null erroresContexto
                                then do
                                    ejecutarPrograma ast
                                    exitSuccess
                                -- Si hay errores de contexto, los imprimimos
                                else do
                                    mapM_ putStrLn erroresContexto
                                    exitFailure
        _ -> do
            putStrLn "Error: Debes proporcionar un archivo de entrada. Ejemplo: ./bot <Archivo>"
            exitFailure
