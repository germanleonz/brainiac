{
module SinBrainiac (parse) where
import LexBrainiac 
}

%name      parse 
%tokentype { Token }
%lexer     { lexer } { T_EOF }

%token 

    varT                          { TkIdent $$ }
    numT                          { TkNum $$ } 

    'declare'                     { TkDeclare }
    'execute'                     { TkExecute }
    'while'                       { TkWhile }
    'for'                         { TkFor }
    'from'                        { TkFrom }
    'to'                          { TkTo }
    'do'                          { TkDo }
    'done'                        { TkDone }
    'boolean'                     { TkBoolean } 
    'integer'                     { TkInteger } 
    'tape'                        { TkTape } 
    'if'                          { TkIf }
    'else'                        { TkElse }
    'then'                        { TkThen }
    'end'                         { TkEnd }
    'at'                          { TkAt }
    'read'                        { TkRead }
    'write'                       { TkWrite }
    'true'                        { TkTrue }
    'false'                       { TkFalse }
    ','                           { TkComa }
    '.'                           { TkPunto }
    ';'                           { TkPuntoYComa }
    '('                           { TkParAbre }
    ')'                           { TkParCierra }
    '['                           { TkCorcheteAbre }
    ']'                           { TkCorcheteCierra }
    '{'                           { TkLlaveAbre }
    '}'                           { TkLlaveCierra }
    '::'                          { TkType }
    '+'                           { TkMas }
    '-'                           { TkMenos }
    '*'                           { TkMult }
    '/'                           { TkDiv }
    '%'                           { TkMod }
    '\/\\'                        { TkConjuncion }
    '\/'                          { TkDisyuncion }
    '~'                           { TkNegacion }
    '<'                           { TkMenor }
    '<='                          { TkMenorIgual }
    '>'                           { TkMayor }
    '>='                          { TkMayorIgual }
    '='                           { TkIgual }
    '/='                          { TkDesigual }
    '&'                           { TkConcat }
    '#'                           { TkInspeccion }
    ':='                          { TkAsignacion }

%%

exprs :: { [Exp] }
      : expr                    { [ $1 ] }
      | expr ';' exprs          { $1 : $3 }

expr :: { Exp }
    : varT ':=' expr                                  { Var $1 $3 }
    | 'if' B 'then' expr 'end'                        { Cond $2 }
    | 'if' B 'then' expr 'else' expr 'end'           { Cond2 $2 }
    | 'while' B 'do' expr 'done'                      { RepIndet $2 $4 }
    | 'for' B 'from' expr 'to' expr 'do' expr 'done'  { RepDet $2 $4 $6 $8 }
    | exp1                                            { $1 } 

B : expr '=' expr                  { BinRel Op_Eq  $1 $3 }
  | expr '/=' expr                  { BinRel Op_Neq $1 $3 }
  | expr '>' expr                   { BinRel Op_Gt  $1 $3 }
  | expr '<' expr                   { BinRel Op_Lt  $1 $3 }
  | expr '>=' expr                  { BinRel Op_Geq $1 $3 }
  | expr '<=' expr                  { BinRel Op_Leq $1 $3 }
  | expr                            { $1 }

exp1 : exp1 '+' Term          { BinArit Suma $1 $3 }
     | exp1 '-' Term          { BinArit Resta $1 $3 }
     | Term                   { Term $1 }

Term : Term '*' Factor        { Times $1 $3 }
     | Term '/' Factor        { Div $1 $3 }
     | Factor                 { Factor $1 }

Factor : numT                 { $1 }
       | varT                 { $1 }
       | 'true'               { $1 }
       | 'false'              { $1 }
       | '(' expr ')'         { Brack $2 }

{
/*Estructura de datos que representa un programa en brainiac*/

data Exp  = Var String Exp |
            Exp1 Exp1 |
            If Cond |
            IfElse Cond2 |
            While RepIndet |
            For RepDet |

data Exp1 = Plus Exp1 Term |
            Minus Exp1 Term |
            Term Term 

data Term = Times Term Factor |
            Div Term Factor |
            Factor Factor 

data Factor = Entero Int |
              Var String |
              Bool Bool |
              Brack Exp

data Cond = Cond  BinRelacional Expr 

data Cond2 = Cond2 BinRelacional Expr Expr

data RepIndet = RepIndet BinRelacional Instr |

data RepDet = RepDet Exp Exp Exp Exp Exp 

data BinRelacional = BinRel CompOp Exp Exp

data BinAritmetico = BinArit OpBin Exp Exp

data OpComp = O_Eq | O_Neq | O_Leq | O_Lt | O_Geq | O_Gt 

data OpBin = Suma | Resta | Mult | Div | Mod

data OpCinta = Concat | Inspec | Ejec

/*Funcion de error*/

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

/*Parser*/
parse = calc . lexer
}
