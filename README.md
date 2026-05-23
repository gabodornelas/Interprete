# Analizador Léxico (LexBot)

Este proyecto implementa un analizador léxico escrito en Haskell. Utiliza la herramienta **Alex** para generar el código encargado de leer un archivo de texto fuente y convertir su contenido en una secuencia de tokens estructurados.

## 🛠️ Requisitos Previos

Para poder compilar y ejecutar este proyecto, necesita tener instaladas las siguientes herramientas en su sistema:

1. **GHC (Glasgow Haskell Compiler):** El compilador estándar de Haskell.
2. **Alex:** El generador de analizadores léxicos para Haskell.

Si tienes `cabal` (el gestor de paquetes de Haskell) instalado, puedes instalar Alex ejecutando el siguiente comando en tu terminal:
`cabal install alex`

---

## 📂 Estructura del Proyecto

* `Reglas.x`: Es el archivo principal de reglas léxicas. Aquí se definen las expresiones regulares y las acciones semánticas.
* `Tokens.hs`: Contiene la definición de los tipos de datos para los tokens (ej. `Token`, `TokenClass`) y la lógica base de cómo representarlos.
* `Main.hs`: Es el programa principal. Lee el archivo de entrada, llama al lexer para procesarlo, y maneja la lógica de validación e impresión (lista de válidos o de errores).

---

## ⚙️ Pasos para Compilar

El proceso de compilación consta de dos fases. Como usamos Alex, primero debemos traducir nuestras reglas (`.x`) a código Haskell puro (`.hs`) antes de compilar el programa final.

Abre la terminal en la carpeta del proyecto y ejecuta estos pasos en orden:

### Paso 1: Generar el Lexer
Ejecute la herramienta Alex sobre el archivo de reglas:
```bash
alex Reglas.x
```
### Paso 2: Compilar el Ejecutable
Use el compilador de Haskell (GHC) indicando que el punto de entrada es el Main.hs. Le daremos el nombre LexBot al archivo ejecutable final:

```Bash
ghc Main.hs -o LexBot
```

### Paso 3: 🚀 Ejecución
Una vez compilado, el programa espera recibir exactamente un argumento: la ruta del archivo de texto que quiere analizar.

Para ejecutarlo, use el siguiente comando:

En Linux

```Bash
./LexBot prueba.bot
```