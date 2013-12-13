{
module SinBrainiac (parse) where
import LexBrainiac 
}

%name      parse 
%tokentype { Token }
%lexer     { lexer } { T_EOF }

%token 

    $digit                        { TkIdent }
    $digit                        { TkNum }
    boolT                         { TkBoolean $$ } 
    intT                          { TkInteger $$ } 

    'declare'                     { TkDeclare }
    'execute'                     { TkExecute }
    'while'                       { TkWhile }
    'from'                        { TkFrom }
    'to'                          { TkTo }
    'do'                          { TkDo }
    'done'                        { TkDone }
    'tape'                        { TkTape $$ } 
    'if'                          { TkIf }
    'else'                        { TkElse }
    'end'                         { TkEnd }
    'at'                          { TkAt }
    'read'                        { TkRead }
    'write'                       { TkWrite }
    'true'                        { TkTrue }
    'false'                       { TkFalse }
    ","                           { TkComa }
    "."                           { TkPunto }
    ";"                           { TkPuntoYComa }
    "("                           { TkParAbre }
    ")"                           { TkParCierra }
    "["                           { TkCorcheteAbre }
    "]"                           { TkCorcheteCierra }
    "{"                           { TkLlaveAbre }
    "}"                           { TkLlaveCierra }
    "::"                          { TkType }
    "+"                           { TkMas }
    "-"                           { TkMenos }
    "*"                           { TkMult }
    "/"                           { TkDiv }
    "%"                           { TkMod }
    "/\"                          { TkConjuncion }
    "\/"                          { TkDisyuncion }
    "~"                           { TkNegacion }
    "<"                           { TkMenor }
    "<="                          { TkMenorIgual }
    ">"                           { TkMayor }
    ">="                          { TkMayorIgual }
    "="                           { TkIgual }
    "\="                          { TkDesigual }
    "&"                           { TkConcat }
    "#"                           { TkInspeccion }
    ":="                          { TkAsignacion }

%%

exprs :: { [Expr] }
       : expr                    { [ $1 ] }
       | expr ';' exprs          { $1 : $3 }

exp :: { Exp }
    : var ":=" Exp in Exp        { Let $2 $4 $6 }
    | Exp1                       { Exp1 $1 }

INSTRUCCION :: { Instruccion }
            : var ":=" Exp     { Asig Nombre  }
            | 'if' if_clauses 'end' { E_If $2 }

ASIGNACION :: { Asignacion }

BIN_EXPRESION :: {  }
               : 

B :: { Seq (Ptr Exp) }
   : E "==" E                  { mkOp $1 Equ $3 }
   | E "/=" E                  { mkOp $1 NEq $3 }
   | E ">" E                   { mkOp $1 GT $3 }
   | E "<" E                   { mkOp $1 LT $3 }
   | E ">=" E                  { mkOp $1 GTE $3 }
   | E "<=" E                  { mkOp $1 LTE $3 }
   | E                         { $1 }

Exp1 : Exp1 '+' Term                { Plus $1 $3 }
     | Exp1 '-' Term                { Minus $1 $3 }
     | Term                        { Term $1 }

Term : Term '*' Factor        { Times $1 $3 }
     | Term '/' Factor        { Div $1 $3 }
     | Factor                 { Factor $1 }

Factor : intT                       { TkNum $1 }
       | varT                       { TkIdent $1 }
       | boolT                      { TkBool $1 }
       | '(' Exp ')'                { Brack $2 }

CONDICIONAL :: { Condicional }
            : guardias 

comp_op :: { CompOp }
        : '=='                                { O_Eq }
        | '/='                                { O_Neq }
        | '<='                                { O_Leq }
        | '<'                                { O_Lt }
        | '>='                                { O_Geq }
        | '>'                                { O_Gt }

expr0 :: { Expr }
      : basic_type                                { $1 }
      | '(' expr ')'                                { $2 }
      | 'begin' exprs 'end'                        { E_Block $2 }
      | 'if' if_clauses 'end'                        { E_If $2 }

{
/*Estructura de datos que representa un programa en brainiac*/

data Programa = Prog [Exp]
data Exp  =
       Var String 
    |  BinAritmetico Int Int 
    | Let String Exp Exp | Exp1 Exp1 
data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term 
data Condicional = Cond Guardia Instr
data Rep_Indet = Rep_Indet Cond Instr
data Term = Times Term Factor | Div Term Factor | Factor Factor 
data Factor = Int Int | Var String | Bool Bool | Brack Exp
data CompOp = O_Eq | O_Neq | O_Leq | O_Lt | O_Geq | O_Gt 

/*Funcion de error*/

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

/*Parser*/
parse = calc . lexer
}
