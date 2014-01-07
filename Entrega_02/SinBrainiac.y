{
module SinBrainiac where

import LexBrainiac (lexer, Token(..))
}

%name calc
%tokentype { Token }
%error     { parseError }

%token 

    ident                         { TkIdent _ $$ }
    num                           { TkNum _ $$ } 

    'declare'                     { TkDeclare _ }
    'execute'                     { TkExecute _ }
    'while'                       { TkWhile _ }
    'for'                         { TkFor _ }
    'from'                        { TkFrom _ }
    'to'                          { TkTo _ }
    'do'                          { TkDo _ }
    'done'                        { TkDone _ }
    'boolean'                     { TkBoolean _ } 
    'integer'                     { TkInteger _ } 
    'tape'                        { TkTape _ } 
    'if'                          { TkIf _ }
    'else'                        { TkElse _ }
    'then'                        { TkThen _ }
    'at'                          { TkAt _ }
    'read'                        { TkRead _ }
    'write'                       { TkWrite _ }
    'true'                        { TkTrue _ }
    'false'                       { TkFalse _ }
    ','                           { TkComa _ }
    '.'                           { TkPunto _ }
    ';'                           { TkPuntoYComa _ }
    '('                           { TkParAbre _ }
    ')'                           { TkParCierra _ }
    '['                           { TkCorcheteAbre _ }
    ']'                           { TkCorcheteCierra _ }
    '{'                           { TkLlaveAbre _ }
    '}'                           { TkLlaveCierra _ }
    '::'                          { TkType _ }
    '+'                           { TkMas _ }
    '-'                           { TkMenos _ }
    '*'                           { TkMult _ }
    '/'                           { TkDiv _ }
    '%'                           { TkMod _ }
    '\/\\'                        { TkConjuncion _ }
    '\/'                          { TkDisyuncion _ }
    '~'                           { TkNegacion _ }
    '<'                           { TkMenor _ }
    '<='                          { TkMenorIgual _ }
    '>'                           { TkMayor _ }
    '>='                          { TkMayorIgual _ }
    '='                           { TkIgual _ }
    '/='                          { TkDesigual _ }
    '&'                           { TkConcat _ }
    '#'                           { TkInspeccion _ }
    ':='                          { TkAsignacion _ }

%%

Prog :: { Programa }
     : 'declare' ListDeclare 'execute' Insts 'done'   { $4 }

ListDeclare :: { [Declaracion] }
            : VarDecla                                { [$1] }
            | VarDecla ';' ListDeclare                { $1 : $3 }

VarDecla :: { Declaracion }
         : VarList '::' Tipo                          { Decl $3 $1 }

VarList :: { [VarName] }
        : ident                                       { [$1] }
        | ident ',' VarList                           { $1 : $3 }

Tipo :: { Tipo }
Tipo : 'boolean'                                      { Tipo_Boolean }
     | 'integer'                                      { Tipo_Integer }
     | 'tape'                                         { Tipo_Tape }

Insts :: { [Inst] }
Insts : I                                             { [$1] }                         
      | I ';' Insts                                   { $1 : $3 }

I :: { Inst }
I : ident ':=' E                                      { I_Assign $1 $3 }
  | 'if' B 'then' I 'done'                            { I_If $2 $4 }
  | 'if' B 'then' I 'else' I 'done'                   { I_IfElse $2 $4 $6 }
  | 'while' B 'do' I 'done'                           { I_While $2 $4 }
  | 'for' ident 'from' E 'to' E 'do' I 'done'         { I_For $2 $4 $6 $8 }
  | 'from' E 'to' E 'do' I 'done'                     { I_From $2 $4 $6 }
  | 'declare' ListDeclare 'execute' Insts 'done'      { I_Declare $4 }
  | 'write' ident                                     { I_Write $2 }
  | 'read' E                                          { I_Read $2 }

B :: { Exp }
  : E '=' E                                           { E_Igual $1 $3 }
  | E '/=' E                                          { E_NoIgual $1 $3 }
  | E '>' E                                           { E_Mayor $1 $3 }
  | E '>=' E                                          { E_MayorI $1 $3 }
  | E '<' E                                           { E_Menor $1 $3 }
  | E '<=' E                                          { E_MenorI $1 $3 }

E :: { Exp }
  : E '+' T                                           { E_Suma  $1 $3 }
  | E '-' T                                           { E_Resta $1 $3 }
  | T                                                 { $1 }

T :: { Exp }
  : T '*' F                                           { E_Mult $1 $3 }
  | T '/' F                                           { E_Div  $1 $3 }
  | T '%' F                                           { E_Mod  $1 $3 }
  | F                                                 { $1 }

F :: { Exp }
  : ident                                             { E_Var $1 } 
  | num                                               { E_Const $1 }

{

--
-- Estructura de datos que representa un programa en Brainiac
--

type Programa = [Inst]

type VarName = String

type Valor = Int

data Tipo = Tipo_Boolean | Tipo_Integer | Tipo_Tape

data Declaracion = Decl Tipo [VarName]

data Inst = I_Assign VarName Exp
          | I_If Exp Inst 
          | I_IfElse Exp Inst Inst
          | I_While Exp Inst
          | I_For VarName Exp Exp Inst
          | I_From Exp Exp Inst
          | I_Declare Programa
          | I_Write VarName
          | I_Read Exp
          deriving (Show)

data Exp = E_Const Valor 
         | E_Var VarName 
         | E_Igual Exp Exp 
         | E_NoIgual Exp Exp 
         | E_Mayor  Exp Exp
         | E_MayorI Exp Exp
         | E_Menor Exp Exp
         | E_MenorI Exp Exp
         | E_Suma  Exp Exp
         | E_Resta Exp Exp
         | E_Mult  Exp Exp
         | E_Div Exp Exp
         | E_Mod Exp Exp
         deriving (Show)

data OpComp = Op_Eq
            | Op_Neq
            | Op_Leq 
            | Op_Lt 
            | Op_Geq 
            | Op_Gt
            deriving (Show)

data OpBinSum  = Op_Suma 
               | Op_Resta
               deriving (Show)

data OpBinMult = Op_Mult 
               | Op_Div 
               | Op_Mod
               deriving (Show)

data OpCinta = Op_Concat
             | Op_Inspec 
             | Op_Ejec 
             deriving (Show)

-- Funcion de error

parseError :: [Token] -> a
parseError t = error ("Parse error \n" ++ (show t))

runCalc :: String -> Programa
runCalc = calc . lexer
}
