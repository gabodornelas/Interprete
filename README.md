# Intérprete

Este proyecto implementa un intérprete dividido por etapas (analizador léxico, analizador sintáctico, análisis de contexto, ...), desarrollado en haskell con la ayuda de diferentes herramientas (alex, happy, ...).

## 🛠️ Requisitos Previos

Para poder compilar y ejecutar este proyecto, necesita tener instaladas las siguientes herramientas en su sistema:

1. **GHC (Glasgow Haskell Compiler):** El compilador estándar de Haskell.
2. **Alex:** El generador de analizadores léxicos para Haskell.
3. **Happy:** El generador de analizadores sintácticos para Haskell.
4. **mtl:** Librería que provee el monad `State`, para el análisis de contexto.

Si tienes `cabal` (el gestor de paquetes de Haskell) instalado, puedes instalar Alex, Happy y mtl ejecutando los siguientes comandos en tu terminal:
`cabal install alex`,
`cabal install happy`,
`cabal install mtl`

---

## 📂 Estructura del Proyecto

* `Reglas.x`: Es el archivo principal de reglas léxicas. Aquí se definen las expresiones regulares y las acciones semánticas.
* `Tokens.hs`: Contiene la definición de los tipos de datos para los tokens (ej. `Token`, `TokenClass`) y la lógica base de cómo representarlos.
* `Main.hs`: Es el programa principal de la etapa 1. Lee el archivo de entrada, llama a las reglas para procesarlo, y maneja la lógica de validación e impresión (lista de válidos o de errores).
* `Sintaxis.y`: Es el archivo principal de reglas sintácticas. Recibe los tokens y aquí se definen las gramáticas.
* `AST.hs`: Contiene la definición de clases de tipos (`NodoAST`) y los tipos de datos (ej. `Program`, `Decl`) para el Árbol Sintáctico Abstracto (Abstract Sintactic Tree) y la lógica de cómo representarlos.
* `Main2.hs`: Es el programa principal de la etapa 2. Lee el archivo de entrada, procesa los tokens y maneja la lógica de validación. Luego analiza la sintáxisy, de ser correcta, muestra el Árbol Sintáctico Abstracto o el error sintáctico.
* `Contexto.hs`: Contiene el analizador de contexto. Implementa la tabla de símbolos jerárquica (mediante `Data.Map` y el monad `State`), la verificación de variables no declaradas o redeclaradas, el uso indebido de la palabra reservada `me`, y la verificación de tipos de expresiones e instrucciones.
* `Main3.hs`: Es el programa principal de la etapa 3. Lee el archivo de entrada, procesa los tokens y maneja la lógica de validación. Luego analiza la sintaxis y, de ser correcta, ejecuta el análisis de contexto sobre el AST. Muestra los errores léxicos, el primer error sintáctico, todos los errores de contexto encontrados, o el AST si el programa es válido.

---

## ⚙️ Pasos para Compilar

El proceso de compilación consta de dos fases. Como usamos Alex y Happy, primero debemos traducir nuestras reglas (`.x` y `.y`) a código Haskell puro (`.hs`) antes de compilar el programa final.

Abre la terminal en la carpeta del proyecto y ejecuta estos pasos en orden:

### Paso 1: Generar las Relgas.hs
Ejecute la herramienta Alex sobre el archivo de reglas:
```bash
alex Reglas.x
```

### Paso 2: Generar las Sintaxis.hs
Ejecute la herramienta Happy sobre el archivo de sintaxis con el flag `--ghc`:
```bash
happy Sintaxis.y --ghc
```

### Paso 3: Compilar el Ejecutable
Use el compilador de Haskell (GHC) indicando que el punto de entrada es el Main.hs. Le daremos el nombre LexBot, SintBot o ContBot al archivo ejecutable final, según la etapa:

* Para la etapa1:
```Bash
ghc Main.hs -o LexBot
```
* Para la etapa2:
```Bash
ghc Main2.hs -o SintBot
```
* Para la etapa3:
```Bash
ghc Main3.hs -o ContBot
```
GHC detecta automáticamente que `Main3.hs` importa `Contexto`, `Sintaxis`, `Reglas` y `Tokens`, y compila esos módulos (junto con `AST.hs`) siempre que estén en la misma carpeta. No hace falta invocar Alex/Happy de nuevo si ya lo hiciste en los Pasos 1 y 2 para esta misma carpeta de trabajo.

### Paso 4: 🚀 Ejecución
Una vez compilado, el programa espera recibir exactamente un argumento: la ruta del archivo de texto que quiere analizar.

Para ejecutarlo, use el siguiente comando:

En Linux

```Bash
./LexBot prueba.bot
```
O
```Bash
./SintBot prueba.bot
```
O
```Bash
./ContBot prueba.bot
```