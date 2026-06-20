# Intérprete

Este proyecto implementa un intérprete dividido por etapas (analizador léxico, analizador sintáctico, ...), desarrollado en haskell con la ayuda de diferentes herramientas (alex, happy, ...).

## 🛠️ Requisitos Previos

Para poder compilar y ejecutar este proyecto, necesita tener instaladas las siguientes herramientas en su sistema:

1. **GHC (Glasgow Haskell Compiler):** El compilador estándar de Haskell.
2. **Alex:** El generador de analizadores léxicos para Haskell.
3. **Happy:** El generador de analizadores sintácticos para Haskell.

Si tienes `cabal` (el gestor de paquetes de Haskell) instalado, puedes instalar Alex y Happy ejecutando los siguientes comandos en tu terminal:
`cabal install alex`,
`cabal install happy`

---

## 📂 Estructura del Proyecto

* `Reglas.x`: Es el archivo principal de reglas léxicas. Aquí se definen las expresiones regulares y las acciones semánticas.
* `Tokens.hs`: Contiene la definición de los tipos de datos para los tokens (ej. `Token`, `TokenClass`) y la lógica base de cómo representarlos.
* `Main.hs`: Es el programa principal de la etapa 1. Lee el archivo de entrada, llama a las reglas para procesarlo, y maneja la lógica de validación e impresión (lista de válidos o de errores).
* `Sintaxis.y`: Es el archivo principal de reglas sintácticas. Recibe los tokens y aquí se definen las gramáticas.
* `AST.hs`: Contiene la definición de clases de tipos (`NodoAST`) y los tipos de datos (ej. `Program`, `Decl`) para el Árbol Sintáctico Abstracto (Abstract Sintactic Tree) y la lógica de cómo representarlos.
* `Main2.hs`: Es el programa principal de la etapa 2. Lee el archivo de entrada, procesa los tokens, y maneja la lógica de validación. Luego analiza la sintáxis y muestra el Árbol Sintáctico Abstracto o el error sintáctico.
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
Use el compilador de Haskell (GHC) indicando que el punto de entrada es el Main.hs. Le daremos el nombre LexBot o SintBot al archivo ejecutable final, según la etapa:

* Para la etapa1:
```Bash
ghc Main.hs -o LexBot
```
* Para la etapa2:
```Bash
ghc Main2.hs -o SintBot
```

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