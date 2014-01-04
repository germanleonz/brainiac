{
module SinBrainiac (parse) where
import LexBrainiac 
}

%name parse
%tokentype { Token }
%lexer     { lexer2 } { TkEOF }

%token 

    varT                          { TkIdent _ $$ }
    numT                          { TkNum _ $$ } 

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
    'end'                         { TkEnd _ }
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

P :: { [Exp] }
      : B                     { [ $1 ] }
      | B ';' P               { $1 : $3 }

B :: { Exp } 
B : varT ':=' E                                   { E_Var $1 $3 }
  | 'if' A 'then' B 'end'                         { E_If $2 $4 }
  | 'if' A 'then' B 'else' B 'end'                { E_IfElse $2 $4 $6 }
  | 'while' A 'do' B 'done'                       { E_While $2 $4 }
  | 'for' E 'from' E 'to' E 'do' B 'done'         { E_For $2 $4 $6 $8 }
  | E                                             { $1 }

A : E '=' E                                       { E_BinRel Op_Eq  $1 $3 }
  | E '/=' E                                      { E_BinRel Op_Neq $1 $3 }
  | E '>' E                                       { E_BinRel Op_Gt  $1 $3 }
  | E '<' E                                       { E_BinRel Op_Lt  $1 $3 }
  | E '>=' E                                      { E_BinRel Op_Geq $1 $3 }
  | E '<=' E                                      { E_BinRel Op_Leq $1 $3 }

E :: { Exp }
E : T                      { E_Term $1 }

T :: { Term } 
T :F { T_Factor $1 }

F :: { Factor }
F : numT         { Fact $1 }

{
/*Estructura de datos que representa un programa en brainiac*/

data Exp = E_Var String Exp
         | E_If Exp Exp 
         | E_IfElse Exp Exp Exp
         | E_While Exp Exp 
         | E_For Exp Exp Exp Exp 
         | E_BinArit OpBin Term Term
         | E_BinRel OpComp Exp Exp
         | E_Term Term

data Term = T_Factor Factor

data Factor = Fact Int 

data OpComp = Op_Eq | Op_Neq | Op_Leq | Op_Lt | Op_Geq | Op_Gt 

data OpBin = Op_Suma | Op_Resta | Op_Mult | Op_Div | Op_Mod

data OpCinta = Concat | Inspec | Ejec

/*Funcion de error*/

happyError _ = error ("Parse error\n")
}
