{
module SinBrainiac where

import Data.Sequence

import LexBrainiac 
import Language

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
     : 'declare' Ds 'execute' Is 'done'   { IDeclare $2 $4    }
     | 'execute' Is 'done'                { IDeclare empty $2 }

Ds :: { Seq Declaracion }
    : VDecl                     { $1       }
    | Ds ';' VDecl              { $1 >< $3 }

VDecl :: { Seq Declaracion }
       : VList '::' Tipo                { fmap (\v -> Decl v $3) $1 }

VList :: { Seq VarName }
       : ident                          { singleton $1 }
       | VList ',' ident                { $1 |> $3 }

Tipo :: { Tipo }
     : 'boolean'                          { TipoBoolean }
     | 'integer'                          { TipoInteger }
     | 'tape'                             { TipoTape }

OpComp :: { OpComp }                          
        : '='                                { OpEq }
        | '/='                               { OpNeq }
        | '<='                               { OpLeq }
        | '<'                                { OpLt }
        | '>='                               { OpGeq }
        | '>'                                { OpGt }

AddOp :: { OpBin }
        : '+'                                { OpSum }
        | '-'                                { OpRes }
        | '/\\'                              { OpCon }
        | '\\/'                              { OpDis }
                                                
MultOp :: { OpBin }                            
        : '*'                                { OpMul }
        | '/'                                { OpDiv }
        | '%'                                { OpMod }
                                                
PrefixOp :: { OpUn }                          
          : '-'                              { OpNegArit }
          | '~'                              { OpNegBool }
          | '#'                              { OpInspecc }

Is :: { Seq Inst }
      : I                                           { singleton $1 }                         
      | Is ';' I                                    { $1 |> $3     }

I :: { Inst }
  : ident ':=' B                                      { IAssign $1 $3 }
  | 'if' B 'then' Is 'done'                           { IIf $2 $4 }
  | 'if' B 'then' Is 'else' Is 'done'                 { IIfElse $2 $4 $6 }
  | 'while' B 'do' Is 'done'                          { IWhile $2 $4 }
  | 'for' ident 'from' B 'to' B 'do' Is 'done'        { IFor $2 $4 $6 $8 }
  | 'for' B 'to' B 'do' Is 'done'                     { IFrom $2 $4 $6 }
  | 'declare' Ds 'execute' Is 'done'                  { IDeclare $2 $4 }
  | 'execute' Is 'done'                               { IDeclare empty $2 }
  | 'write' B                                         { IWrite $2 }
  | 'read' ident                                      { IRead $2 }
  | '{' cadena '}' 'at' B                             { IEjec $2 $5 }
  | B '&' B                                           { IConcat $1 $3 }

B :: { Exp }
  : B OpComp E                                        { EComp $2 $1 $3 }
  | E                                                 { $1 }
                                                      
E :: { Exp }                                          
  : E AddOp T                                         { EBinOp $2 $1 $3 }
  | T                                                 { $1 }
                                                      
T :: { Exp }                                          
  : T MultOp F                                        { EBinOp $2 $1 $3}
  | F                                                 { $1 }
                                                      
F :: { Exp }                                          
  : ident                                             { EVar $1 } 
  | num                                               { EConst $1 }
  | PrefixOp F                                        { EUnOp $1 $2 }
  | 'true'                                            { ETrue }
  | 'false'                                           { EFalse }
  | '[' B ']'                                         { ECorch $2 }   
  | '(' B ')'                                         { EParen $2 }     
                                                      
cadena :: { singleton BInst }                         
       : BInst                                        { singleton $1 }
       | cadena BInst                                 { $1 |> $2     }

BInst :: { BInst }
       : '+'                    { CSum }
       | '-'                    { CRes }
       | '<'                    { CIzq }
       | '>'                    { CDer }
       | '.'                    { CImp }
       | ','                    { CLee }

{
--
-- Funcion de error
--
parseError :: [Token] -> a
parseError tks = error $ "Error sintactico, " ++
    "Simbolo inesperado \"" ++ show (head tks) ++ "\""

}
