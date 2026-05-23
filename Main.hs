module Main where

import System.Environment (getArgs)                 -- Para los argumentos que trae el ejecutable (el archivo)
import System.Exit (exitFailure)                    -- Para terminar el programa
import Data.List (partition, intercalate)           -- Importamos la herramienta para dividir listas y la herramienta para imprimir de forma intercalada
import Lexer (alexScanTokens)                       -- El analizador
import Tokens                                       -- Los Tokens que definimos

-- Función que solo convierte el token a texto (para imprimirlo)
formatToken :: Token -> String
formatToken (Token fila col clase) = 
    show clase ++ " " ++ show fila ++ " " ++ show col

-- Función que dice si un Token es de clase TkError o no
esError :: Token -> Bool
esError (Token _ _ (TkError _)) = True
esError _                       = False

-- Imprime los errores con el formato indicado
printError :: Token -> IO ()
printError (Token fila col (TkError c)) =
    putStrLn $ "Error: Caracter inesperado \"" ++ [c] ++ "\" en la fila " ++ show fila ++ ", columna " ++ show col ++ "."
printError _ = return ()

-- Bloque principal
main :: IO ()
main = do
    args <- getArgs
    
    case args of -- Exactamente un archivo
        [archivo] -> do
            contenido <- readFile archivo
            
            -- Generamos la lista completa (mezclada con válidos y errores)
            let tokens = alexScanTokens contenido
            
            -- Dividimos la lista en dos. 'partition' recibe la condición (esError) y la lista original
            let (errores, validos) = partition esError tokens
            
            -- Verificamos si la lista de errores está vacía
            if null errores
                then do
                    -- Si está vacía, no hay errores, imprimimos los válidos
                   putStrLn $ intercalate ", " (map formatToken validos)
                else do
                    -- Si no está vacía, hay errores. Los imprimimos y salimos.
                    mapM_ printError errores  -- Mapeamos la lista de errores y le aplicamos printError a cada elemento
                    exitFailure

        _ -> do -- nada o mas de un archivo
            putStrLn "Error: Debes proporcionar un archivo de entrada. Ejemplo de ejecucion: ./LexBot <Archivo>"
            exitFailure
        