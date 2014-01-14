{
module SinBrainiac where

import LexBrainiac 
import Control.Monad.State
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
    'do'                          { TkDo _ }
    'for'                         { TkFor _ }
    'from'                        { TkFrom _ }
    'to'                          { TkTo _ }
    'if'                          { TkIf _ }
    'then'                        { TkThen _ }
    'else'                        { TkElse _ }
    'read'                        { TkRead _ }
    'write'                       { TkWrite _ }
    'boolean'                     { TkBoolean _ } 
    'integer'                     { TkInteger _ } 
    'tape'                        { TkTape _ } 
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

    '['                           { TkCorcheteAbre _ }
    ']'                           { TkCorcheteCierra _ }
    '{'                           { TkLlaveAbre _ }
    '}'                           { TkLlaveCierra _ }
    'at'                          { TkAt _ }
    '&'                           { TkConcat _ }
    '#'                           { TkInspeccion _ }
    ':='                          { TkAsignacion _ }

%%

AST :: { Inst }
     : 'declare' Ds 'execute' Is 'done'   { I_Declare $2 $4 }

Ds :: { [Declaracion] }
            : V_Decl                      { $1 }
            | V_Decl ';' Ds               { $1 ++ $3 }

V_Decl :: { [Declaracion] }
         : V_List '::' Tipo               { map (\v -> Decl v $3) $1 }

V_List :: { [VarName] }
        : ident                           { [$1] }
        | ident ',' V_List                { $1 : $3 }

Tipo :: { Tipo }
     : 'boolean'                          { Tipo_Boolean }
     | 'integer'                          { Tipo_Integer }
     | 'tape'                             { Tipo_Tape }

Comp_op :: { OpComp }                          
        : '='                                { Op_Eq }
        | '/='                               { Op_Neq }
        | '<='                               { Op_Leq }
        | '<'                                { Op_Lt }
        | '>='                               { Op_Geq }
        | '>'                                { Op_Gt }

Add_op :: { OpBin }
        : '+'                                { Op_Sum }
        | '-'                                { Op_Res }
        | '/\\'                              { Op_Con }
        | '\\/'                              { Op_Dis }
                                                
Mult_op :: { OpBin }                            
        : '*'                                { Op_Mul }
        | '/'                                { Op_Div }
        | '%'                                { Op_Mod }
                                                
Prefix_op :: { OpUn }                          
          : '-'                              { Op_NegArit }
          | '~'                              { Op_NegBool }
          | '#'                              { Op_Inspecc }
   
Is :: { [Inst] }
      : I                                             { [$1] }                         
      | I ';' Is                                      { $1 : $3 }

I :: { Inst }
  : ident ':=' E                                      { I_Assign $1 $3 }
  | 'if' B 'then' Is 'done'                           { I_If $2 $4 }
  | 'if' B 'then' Is 'else' Is 'done'                 { I_IfElse $2 $4 $6 }
  | 'while' B 'do' Is 'done'                          { I_While $2 $4 }
  | 'for' ident 'from' E 'to' E 'do' Is 'done'        { I_For $2 $4 $6 $8 }
  | 'from' E 'to' E 'do' Is 'done'                    { I_From $2 $4 $6 }
  | 'declare' Ds 'execute' Is 'done'                  { I_Declare $2 $4 }
  | 'write' E                                         { I_Write $2 }
  | 'read' ident                                      { I_Read $2 }
  | '{' cadena '}' 'at' E                             { I_Ejec $2 $5 }
  | E '&' E                                           { I_Concat $1 $3 }

B :: { Exp }
  : E Comp_op E                                       { E_Comp $2 $1 $3 }

E :: { Exp }
  : E Add_op T                                        { E_BinOp $2 $1 $3 }
  | T                                                 { $1 }

T :: { Exp }
  : T Mult_op F                                       { E_BinOp $2 $1 $3}
  | F                                                 { $1 }

F :: { Exp }
  : ident                                             { E_Var $1 } 
  | num                                               { E_Const $1 }
  | Prefix_op F                                       { E_UnOp $1 $2 }
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
-- Tipos de datos que representan un programa en Brainiac
--

type VarName = String
type Valor = Int

data Declaracion = Decl VarName Tipo deriving (Show)

data Inst = I_Assign VarName Exp
          | I_If Exp [Inst] 
          | I_IfElse Exp [Inst] [Inst]
          | I_While Exp [Inst]
          | I_For VarName Exp Exp [Inst]
          | I_From Exp Exp [Inst]
          | I_Declare [Declaracion] [Inst]
          | I_Write Exp
          | I_Read VarName
          | I_Ejec [B_Inst] Exp
          | I_Concat Exp Exp

data Exp = E_Const Valor 
         | E_Var VarName 
         | E_True 
         | E_False 
         | E_BinOp OpBin Exp Exp 
         | E_Comp OpComp Exp Exp 
         | E_UnOp OpUn Exp 
         | E_Paren Exp 
         | E_Corch Exp
        deriving (Show)

data B_Inst = C_Sum
            | C_Res
            | C_Izq
            | C_Der
            | C_Imp
            | C_Lee
            deriving (Show)

data OpBin = Op_Sum 
           | Op_Res
           | Op_Mul 
           | Op_Div 
           | Op_Mod
           | Op_Con
           | Op_Dis

data OpComp = Op_Eq
            | Op_Neq
            | Op_Lt 
            | Op_Leq 
            | Op_Gt
            | Op_Geq 

data OpUn = Op_NegArit
          | Op_NegBool
          | Op_Inspecc

data Tipo = Tipo_Boolean
          | Tipo_Integer 
          | Tipo_Tape
          deriving (Eq, Show)

instance Show OpBin where
    show Op_Sum = "'Suma'"
    show Op_Res = "'Resta'"
    show Op_Mul = "'Multiplicacion'"
    show Op_Div = "'Division'"
    show Op_Mod = "'Modulo'"
    show Op_Dis = "'Disyuncion'"
    show Op_Con = "'Conjuncion'"

instance Show OpComp where
    show Op_Eq  = "'Igual'"
    show Op_Neq = "'No Igual'"
    show Op_Lt  = "'Menor que'"
    show Op_Leq = "'Menor o igual'"
    show Op_Gt  = "'Mayor que'"
    show Op_Geq = "'Mayor o igual'"

instance Show OpUn where
    show Op_NegArit = "'Negacion Aritmetica'"
    show Op_NegBool = "'Negacion Booleana'"
    show Op_Inspecc = "'Inspeccion'"

--
-- Funcion de error
--
parseError :: [Token] -> a
parseError tks = error $ "Error sintactico, Simbolo inesperado " ++ show (head tks)
}
