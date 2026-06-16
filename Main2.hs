module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.List (partition)
import Reglas (alexScanTokens)
import Tokens
import Parser (parseBot)
import AST (imprimir) -- Importamos la función de la clase de tipos

esError :: Token -> Bool
esError (Token _ _ (TkError _)) = True
esError _                       = False

printError :: Token -> IO ()
printError (Token fila col (TkError c)) =
    putStrLn $ "Error Léxico: Caracter inesperado \"" ++ [c] ++ "\" en la fila " ++ show fila ++ ", columna " ++ show col ++ "."
printError _ = return ()

main :: IO ()
main = do
    args <- getArgs
    
    case args of 
        [archivo] -> do
            contenido <- readFile archivo
            let tokens = alexScanTokens contenido
            
            let (errores, validos) = partition esError tokens
            
            if not (null errores)
                then do
                    mapM_ printError errores
                    exitFailure
                else do
                    -- 1. Analizamos la sintaxis
                    let ast = parseBot validos
                    
                    -- 2. Usamos nuestra Clase de Tipos para imprimir el AST 
                    -- de forma legible (nivel de indentación inicial 0)
                    putStrLn $ imprimir 0 ast
                    
                    exitSuccess

        _ -> do
            putStrLn "Error: Uso: ./SintBot <Archivo>"
            exitFailure