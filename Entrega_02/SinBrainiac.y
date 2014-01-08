{
module SinBrainiac where

import LexBrainiac 
}

%name calc
%tokentype { Token }
%error     { parseError }

%token 

    ident                         { TkIdent _ $$ }
    num                           { TkNum _ $$ } 

    'declare'                     { TkDeclare _ }
    'execute'                     { TkExecute _ }
    'done'                        { TkDone _ }
    'while'                       { TkWhile _ }
    'for'                         { TkFor _ }
    'from'                        { TkFrom _ }
    'to'                          { TkTo _ }
    'do'                          { TkDo _ }
    'boolean'                     { TkBoolean _ } 
    'integer'                     { TkInteger _ } 
    'tape'                        { TkTape _ } 
    'if'                          { TkIf _ }
    'else'                        { TkElse _ }
    'then'                        { TkThen _ }
    'read'                        { TkRead _ }
    'write'                       { TkWrite _ }
    'true'                        { TkTrue _ }
    'false'                       { TkFalse _ }
    ','                           { TkComa _ }
    '.'                           { TkPunto _ }
    ';'                           { TkPuntoYComa _ }
    '('                           { TkParAbre _ }
    ')'                           { TkParCierra _ }
    '::'                          { TkType _ }
    '+'                           { TkMas _ }
    '-'                           { TkMenos _ }
    '*'                           { TkMult _ }
    '/'                           { TkDiv _ }
    '%'                           { TkMod _ }
    '/\\'                         { TkConjuncion _ }
    '\\/'                         { TkDisyuncion _ }
    '~'                           { TkNegacion _ }

    '<'                           { TkMenor _ }
    '<='                          { TkMenorIgual _ }
    '>'                           { TkMayor _ }
    '>='                          { TkMayorIgual _ }
    '='                           { TkIgual _ }
    '/='                          { TkDesigual _ }

    'at'                          { TkAt _ }
    '['                           { TkCorcheteAbre _ }
    ']'                           { TkCorcheteCierra _ }
    '{'                           { TkLlaveAbre _ }
    '}'                           { TkLlaveCierra _ }
    '&'                           { TkConcat _ }
    '#'                           { TkInspeccion _ }
    ':='                          { TkAsignacion _ }

%%

Prog :: { Programa }
     : 'declare' ListDeclare 'execute' IS 'done'   { $4 }

ListDeclare :: { [Declaracion] }
            : VarDecla                                { [$1] }
            | VarDecla ';' ListDeclare                { $1 : $3 }

VarDecla :: { Declaracion }
         : VarList '::' Tipo                          { Decl $3 $1 }

VarList :: { [VarName] }
        : ident                                       { [$1] }
        | ident ',' VarList                           { $1 : $3 }

Tipo :: { Tipo }
     : 'boolean'                                      { Tipo_Boolean }
     | 'integer'                                      { Tipo_Integer }
     | 'tape'                                         { Tipo_Tape }

Add_op :: { OpBin }
        : '+'                                { Op_Sum }
        | '-'                                { Op_Res }
        | '/\\'                              { Op_Con }
        | '\\/'                              { Op_Dis }
        | '\\/'                              { Op_Dis }
                                                
Comp_op :: { OpComp }                          
        : '='                                { Op_Eq }
        | '/='                               { Op_Neq }
        | '<='                               { Op_Leq }
        | '<'                                { Op_Lt }
        | '>='                               { Op_Geq }
        | '>'                                { Op_Gt }
                                                
Mult_op :: { OpBin }                            
        : '*'                                { Op_Mul }
        | '/'                                { Op_Div }
        | '%'                                { Op_Mod }
                                                
Prefix_op :: { OpUn }                          
          : '-'                              { Op_NegArit }
          | '~'                              { Op_NegBool }
          | '#'                              { Op_Inspecc }
   
IS :: { [Inst] }
      : I                                             { [$1] }                         
      | I ';' IS                                      { $1 : $3 }

I :: { Inst }
  : ident ':=' E                                      { I_Assign $1 $3 }
  | 'if' B 'then' IS 'done'                           { I_If $2 $4 }
  | 'if' B 'then' IS 'else' IS 'done'                 { I_IfElse $2 $4 $6 }
  | 'while' B 'do' IS 'done'                          { I_While $2 $4 }
  | 'for' ident 'from' E 'to' E 'do' IS 'done'        { I_For $2 $4 $6 $8 }
  | 'from' E 'to' E 'do' IS 'done'                    { I_From $2 $4 $6 }
  | 'declare' ListDeclare 'execute' IS 'done'         { I_Declare $4 }
  | 'write' ident                                     { I_Write $2 }
  | 'read' E                                          { I_Read $2 }
  | '{' cadena '}' 'at' E                             { I_Ejec $2 $5 }
  | E '&' E                                           { I_Concat $1 $3 }

B :: { BoolExp }
  : E Comp_op E                                       { B_Comp $2 $1 $3 }

E :: { Exp }
  : E Add_op T                                        { E_BinOp $2 $1 $3 }
  | T                                                 { $1 }

T :: { Exp }
  : T Mult_op U                                       { E_BinOp $2 $1 $3 }
  | U                                                 { $1 }

U :: { Exp }
  : Prefix_op F                                       { E_UnOp $1 $2 }
  | F                                                 { $1 }

F :: { Exp }
  : ident                                             { E_Var $1 } 
  | num                                               { E_Const $1 }
  | 'true'                                            { E_True }
  | 'false'                                           { E_False }
  | '[' E ']'                                         { E_Corch $2 }   
  | '(' E ')'                                         { E_Paren $2 }     

cadena :: { [B_Inst] }
       : B_Inst                                       { [$1] }
       | B_Inst cadena                                { $1 : $2 }

B_Inst :: { B_Inst }
       : '+'                    { C_Sum }
       | '-'                    { C_Res }
       | '<'                    { C_Izq }
       | '>'                    { C_Der }
       | '.'                    { C_Imp }
       | ','                    { C_Lee }

{

--
-- Estructura de datos que representa un programa en Brainiac
--

type Programa = [Inst]

type VarName = String

type Valor = Int

data Tipo = Tipo_Boolean
          | Tipo_Integer 
          | Tipo_Tape
          deriving (Show)

data OpBin = Op_Sum 
           | Op_Res
           | Op_Mul 
           | Op_Div 
           | Op_Mod
           | Op_Con
           | Op_Dis
           deriving (Show)

data OpComp = Op_Eq
            | Op_Neq
            | Op_Lt 
            | Op_Leq 
            | Op_Gt
            | Op_Geq 
            deriving (Show)

data OpUn = Op_NegArit
          | Op_NegBool
          | Op_Inspecc
          deriving (Show)

data Declaracion = Decl Tipo [VarName]

data Inst = I_Assign VarName Exp
          | I_If BoolExp Programa 
          | I_IfElse BoolExp Programa Programa
          | I_While BoolExp Programa
          | I_For VarName Exp Exp Programa
          | I_From Exp Exp Programa
          | I_Declare Programa
          | I_Write VarName
          | I_Read Exp
          | I_Ejec [B_Inst] Exp
          | I_Concat Exp Exp
          deriving (Show)

data Exp = E_Const Valor 
         | E_Var VarName 
         | E_True
         | E_False 
         | E_BinOp OpBin Exp Exp
         | E_UnOp OpUn Exp
         | E_Paren Exp
         | E_Corch Exp
         deriving (Show)

data BoolExp = B_Comp OpComp Exp Exp
             deriving (Show)

data B_Inst = C_Sum
            | C_Res
            | C_Izq
            | C_Der
            | C_Imp
            | C_Lee
            deriving (Show)

--
-- Funcion de error
--
parseError :: [Token] -> a
parseError tks = error $ "Error sintactico, Tokens: " ++ (show tks)

runCalc :: String -> Programa
runCalc = calc . lexer
}
