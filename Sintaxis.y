{
module Sintaxis where
import Tokens
import AST
}

%name sintBot
%tokentype { Token }
%error { sintError }

%token
    create       { Token _ _ TkCreate }
    bot          { Token _ _ TkBot }
    on           { Token _ _ TkOn }
    activation   { Token _ _ TkActivation }
    deactivation { Token _ _ TkDeactivation }
    default      { Token _ _ TkDefault }
    end          { Token _ _ TkEnd }
    execute      { Token _ _ TkExecute }
    activate     { Token _ _ TkActivate }
    advance      { Token _ _ TkAdvance }
    deactivate   { Token _ _ TkDeactivate }
    if           { Token _ _ TkIf }
    else         { Token _ _ TkElse }
    while        { Token _ _ TkWhile }
    store        { Token _ _ TkStore }
    collect      { Token _ _ TkCollect }
    drop         { Token _ _ TkDrop }
    left         { Token _ _ TkLeft }
    right        { Token _ _ TkRight }
    up           { Token _ _ TkUp }
    down         { Token _ _ TkDown }
    read         { Token _ _ TkRead }
    receive      { Token _ _ TkReceive }
    send         { Token _ _ TkSend }
    as           { Token _ _ TkAs }
    int          { Token _ _ TkInt }
    bool         { Token _ _ TkBool }
    char         { Token _ _ TkChar }
    true         { Token _ _ TkTrue }
    false        { Token _ _ TkFalse }
    me           { Token _ _ TkMe }
    ident        { Token _ _ (TkIdent $$) }         -- Los $$ representan el valor dentro del token
    num          { Token _ _ (TkNum $$) }
    charLit      { Token _ _ (TkCaracter $$) }
    ':'          { Token _ _ TkDosPuntos }
    '.'          { Token _ _ TkPunto }
    ','          { Token _ _ TkComa }
    '('          { Token _ _ TkParAbre }
    ')'          { Token _ _ TkParCierra }
    '>'          { Token _ _ TkMayor }
    '<'          { Token _ _ TkMenor }
    '>='         { Token _ _ TkMayorIgual }
    '<='         { Token _ _ TkMenorIgual }
    '='          { Token _ _ TkIgual }
    '/='         { Token _ _ TkDesigual }
    '+'          { Token _ _ TkSuma }
    '-'          { Token _ _ TkResta }
    '*'          { Token _ _ TkMult }
    '/'          { Token _ _ TkDiv }
    '%'          { Token _ _ TkMod }
    '/\\'        { Token _ _ TkConjuncion }
    '\\/'        { Token _ _ TkDisyuncion }
    '~'          { Token _ _ TkNegacion }

-- Reglas de Precedencia y Asociatividad (Según las tablas de las secciones 3.3 y 3.4 del PDF)
-- Los operadores más fuertes van más abajo.
%left '\\/'
%left '/\\'
%nonassoc '>' '<' '>=' '<=' '=' '/='
%left '+' '-'
%left '*' '/' '%'
%right NEGATIVO '~'

%%

-- Gramaticas

-- Genera declaraciones con instrucciones O solo instrucciones
Program : create Decls execute Instrs end { Program $2 $4 }
        | execute Instrs end              { Program [] $2 }

-- Genera una declaracion concatenada con mas declaraciones O la lista con solo una instruccion
Decls : Decl Decls { $1 : $2 }
      | Decl       { [$1] }

-- Genera una declaracion con su estructura
Decl : Type bot IdList Behaviors end { Decl $1 $3 $4 }

-- Genera un tipo entero, booleano o caracter
Type : int  { TyInt }
     | bool { TyBool }
     | char { TyChar }

-- Genera un ident intercalado con mas idents O una lista con un ident solo 
IdList : ident ',' IdList { $1 : $3 }
       | ident            { [$1] }

-- Genera un comportamiento seguido de mas comportamientos O <vacio>
Behaviors : Behavior Behaviors { $1 : $2 }
          | {- empty -}        { [] }

-- Genera un comportamiento con su estructura
Behavior : on Condition ':' Instrs end { Behavior $2 $4 }

-- Genera activacion O desactivacion O expresion O default
Condition : activation   { OnActivation }
          | deactivation { OnDeactivation }
          | Expr         { OnExpr $1 }
          | default      { OnDefault }

-- Genera una instruccion seguida de mas instrucciones O solo una instruccion
Instrs : Instr Instrs { Seq $1 $2 }
       | Instr        { $1 }

-- En BOT, casi todas las instrucciones simples terminan en punto (.).
-- Las instrucciones de bloque terminan con su respectivo 'end'.
Instr : activate IdList '.'                            { Activate $2 }
      | advance IdList '.'                             { Advance $2 }
      | deactivate IdList '.'                          { Deactivate $2 }
      | if Expr ':' Instrs else ':' Instrs end         { If $2 $4 (Just $7) }
      | if Expr ':' Instrs end                         { If $2 $4 Nothing }
      | while Expr ':' Instrs end                      { While $2 $4 }
    --  | create Decls execute Instrs end                { Scope $2 $4 }  comentamos scope porque no esta especificado para esta entrega
      | store Expr '.'                                 { Store $2 }
      | collect as ident '.'                           { Collect (Just $3) }
      | collect '.'                                    { Collect Nothing }
      | drop Expr '.'                                  { Drop $2 }
      | Dir Expr '.'                                   { Move $1 (Just $2) }
      | Dir '.'                                        { Move $1 Nothing }
      | read as ident '.'                              { Read (Just $3) }
      | read '.'                                       { Read Nothing }
      | receive '.'                                    { Receive }
      | send '.'                                       { Send }

-- Genera izquierda O derecha O arriba O abajo
Dir : left  { DirLeft }
    | right { DirRight }
    | up    { DirUp }
    | down  { DirDown }

Expr : num                     { LitInt $1 }
     | true                    { LitBool True }
     | false                   { LitBool False }
     | charLit                 { LitChar $1 }
     | ident                   { Var $1 }
     | me                      { Var "me" }
     | '(' Expr ')'            { $2 }
     | Expr '+' Expr           { OpBin Add $1 $3 }
     | Expr '-' Expr           { OpBin Sub $1 $3 }
     | Expr '*' Expr           { OpBin Mul $1 $3 }
     | Expr '/' Expr           { OpBin Div $1 $3 }
     | Expr '%' Expr           { OpBin Mod $1 $3 }
     | Expr '/\\' Expr         { OpBin And $1 $3 }
     | Expr '\\/' Expr         { OpBin Or $1 $3 }
     | Expr '<' Expr           { OpBin Lt $1 $3 }
     | Expr '<=' Expr          { OpBin Le $1 $3 }
     | Expr '>' Expr           { OpBin Gt $1 $3 }
     | Expr '>=' Expr          { OpBin Ge $1 $3 }
     | Expr '=' Expr           { OpBin Eq $1 $3 }
     | Expr '/=' Expr          { OpBin Neq $1 $3 }
     | '-' Expr %prec NEGATIVO { OpUn Neg $2 }
     | '~' Expr                { OpUn Not $2 }

{
sintError :: [Token] -> a
sintError [] = error "Error sintáctico: fin de archivo inesperado."
sintError (Token f c cls : _) = 
    error $ "Error sintáctico en fila " ++ show f ++ ", columna " ++ show c ++ " cerca del token " ++ show cls
}