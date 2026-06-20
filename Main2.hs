module Main where

import System.Environment (getArgs)                 -- Para los argumentos que trae el ejecutable (el archivo)
import System.Exit (exitFailure, exitSuccess)       -- Para terminar el programa
import Data.List (partition)                        -- Importamos la herramienta para dividir listas
import Reglas (alexScanTokens)                      -- El analizador
import Tokens                                       -- Los Tokens que definimos
import Sintaxis (sintBot)                           -- La gramatica definida
import AST (imprimir)                               -- Para imprimir el AST

-- Función que dice si un Token es de clase TkError o no
esError :: Token -> Bool
esError (Token _ _ (TkError _)) = True
esError _                       = False

-- Imprime los errores con el formato indicado
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
    args <- getArgs
    
    case args of 
        [archivo] -> do -- Exactamente un archivo
            contenido <- readFile archivo

            -- Generamos la lista completa (mezclada con válidos y errores)
            let tokens = alexScanTokens contenido
            
             -- Dividimos la lista en dos. 'partition' recibe la condición (esError) y la lista original
            let (errores, validos) = partition esError tokens
            
            -- Verificamos si la lista de errores no está vacía
            if not (null errores)
                then do
                    -- Si no está vacía, hay errores léxicos. Los imprimimos y salimos.
                    mapM_ printErrorLex errores
                    exitFailure
                else do
                    -- Si está vacía, no hay errores, analizamos la sintaxis
                    case sintBot validos of
                        -- Si hubo un error sintáctico, recibimos el token mas cercano al conflicto
                        Left tokensError -> do
                            printErrorSint tokensError
                            exitFailure
                            
                        -- Si la sintaxis fue correcta, imprimimos el AST
                        Right ast -> do
                            putStrLn $ imprimir 0 ast
                            exitSuccess

        _ -> do -- nada o mas de un archivo
            putStrLn "Error: Debes proporcionar un archivo de entrada. Ejemplo de ejecucion: ./SintBot <Archivo>"
            exitFailure