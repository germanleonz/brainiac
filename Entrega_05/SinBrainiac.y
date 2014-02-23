{
module SinBrainiac where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS

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
     | 'execute' Is 'done'                { I_Declare [] $2 }

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
  : ident ':=' B                                      { I_Assign $1 $3 }
  | 'if' B 'then' Is 'done'                           { I_If $2 $4 }
  | 'if' B 'then' Is 'else' Is 'done'                 { I_IfElse $2 $4 $6 }
  | 'while' B 'do' Is 'done'                          { I_While $2 $4 }
  | 'for' ident 'from' B 'to' B 'do' Is 'done'        { I_For $2 $4 $6 $8 }
  | 'for' B 'to' B 'do' Is 'done'                     { I_From $2 $4 $6 }
  | 'declare' Ds 'execute' Is 'done'                  { I_Declare $2 $4 }
  | 'execute' Is 'done'                               { I_Declare [] $2 }
  | 'write' B                                         { I_Write $2 }
  | 'read' ident                                      { I_Read $2 }
  | '{' cadena '}' 'at' B                             { I_Ejec $2 $5 }
  | B '&' B                                           { I_Concat $1 $3 }

B :: { Exp }
  : B Comp_op E                                       { E_Comp $2 $1 $3 }
  | E                                                 { $1 }

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
  | '[' B ']'                                         { E_Corch $2 }   
  | '(' B ')'                                         { E_Paren $2 }     

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
type Valor   = Int

data Declaracion = Decl VarName Tipo deriving (Show)

data Inst = I_Assign VarName Exp
          | I_If     Exp [Inst] 
          | I_IfElse Exp [Inst] [Inst]
          | I_While  Exp [Inst]
          | I_For    VarName Exp Exp [Inst]
          | I_From   Exp Exp [Inst]
          | I_Declare [Declaracion] [Inst]
          | I_Write Exp
          | I_Read VarName
          | I_Ejec [B_Inst] Exp
          | I_Concat Exp Exp

instance Show Inst where show = correrImpresor . impresor 

data Exp = E_Const Valor 
         | E_Var   VarName 
         | E_True 
         | E_False 
         | E_BinOp OpBin   Exp Exp 
         | E_Comp  OpComp Exp Exp 
         | E_UnOp  OpUn   Exp 
         | E_Paren Exp 
         | E_Corch Exp

instance Show Exp where show = correrImpresor . impresorE

data B_Inst = C_Sum
            | C_Res
            | C_Izq
            | C_Der
            | C_Imp
            | C_Lee

instance Show B_Inst where
    show (C_Sum) = "+"
    show (C_Res) = "-"
    show (C_Izq) = "<"
    show (C_Der) = ">"
    show (C_Imp) = "."
    show (C_Lee) = ","

data OpBin = Op_Sum 
           | Op_Res
           | Op_Mul 
           | Op_Div 
           | Op_Mod
           | Op_Con
           | Op_Dis

instance Show OpBin where
    show Op_Sum = "'Suma'"
    show Op_Res = "'Resta'"
    show Op_Mul = "'Multiplicacion'"
    show Op_Div = "'Division'"
    show Op_Mod = "'Modulo'"
    show Op_Dis = "'Disyuncion'"
    show Op_Con = "'Conjuncion'"

data OpComp = Op_Eq
            | Op_Neq
            | Op_Lt 
            | Op_Leq 
            | Op_Gt
            | Op_Geq 

instance Show OpComp where
    show Op_Eq  = "'Igual'"
    show Op_Neq = "'No Igual'"
    show Op_Lt  = "'Menor que'"
    show Op_Leq = "'Menor o igual'"
    show Op_Gt  = "'Mayor que'"
    show Op_Geq = "'Mayor o igual'"

data OpUn = Op_NegArit
          | Op_NegBool
          | Op_Inspecc

instance Show OpUn where
    show Op_NegArit = "'Negacion Aritmetica'"
    show Op_NegBool = "'Negacion Booleana'"
    show Op_Inspecc = "'Inspeccion'"

data Tipo = Tipo_Boolean
          | Tipo_Integer 
          | Tipo_Tape
          deriving (Eq)

instance Show Tipo where
    show (Tipo_Integer) = "tipo entero"
    show (Tipo_Boolean) = "tipo boolean"
    show (Tipo_Tape)    = "tipo cinta"

--
-- Funcion de error
--
parseError :: [Token] -> a
parseError tks = error $ "Error sintactico, " ++
    "Simbolo inesperado \"" ++ mostrar (head tks) ++ "\""

mostrar :: Token -> String
mostrar (TkDeclare  _)       = "declare"
mostrar (TkExecute  _)       = "execute"
mostrar (TkWhile    _)       = "while"
mostrar (TkFor      _)       = "for"
mostrar (TkFrom     _)       = "from"
mostrar (TkTo       _)       = "to"
mostrar (TkDo       _)       = "do"
mostrar (TkDone     _)       = "done"
mostrar (TkInteger  _)       = "integer"
mostrar (TkBoolean  _)       = "boolean"
mostrar (TkTape     _)       = "tape"
mostrar (TkIf       _)       = "if"
mostrar (TkElse     _)       = "else"
mostrar (TkThen     _)       = "then"
mostrar (TkAt       _)       = "at"
mostrar (TkRead     _)       = "read"
mostrar (TkWrite    _)       = "write"
mostrar (TkIdent _ id)       = id
mostrar (TkNum   _ n )       = show n
mostrar (TkTrue     _)       = "true"
mostrar (TkFalse    _)       = "false"
mostrar (TkComa     _)       = ","  
mostrar (TkPunto    _)       = "."  
mostrar (TkPuntoYComa     _) = ";"  
mostrar (TkParAbre        _) = "("  
mostrar (TkParCierra      _) = ")"  
mostrar (TkCorcheteAbre   _) = "["  
mostrar (TkCorcheteCierra _) = "]"  
mostrar (TkLlaveAbre      _) = "{"  
mostrar (TkLlaveCierra    _) = "}"  
mostrar (TkType           _) = "::"
mostrar (TkMas            _) = "+"  
mostrar (TkMenos          _) = "-"  
mostrar (TkMult           _) = "*"  
mostrar (TkDiv            _) = "/"  
mostrar (TkMod            _) = "%"  
mostrar (TkConjuncion     _) = "/\\"
mostrar (TkDisyuncion     _) = "\\/"
mostrar (TkNegacion       _) = "~"  
mostrar (TkMenor          _) = "<"  
mostrar (TkMenorIgual     _) = "<="
mostrar (TkMayor          _) = ">" 
mostrar (TkMayorIgual     _) = ">="
mostrar (TkIgual          _) = "=" 
mostrar (TkDesigual       _) = "/="
mostrar (TkConcat         _) = "&" 
mostrar (TkInspeccion     _) = "#"  
mostrar (TkAsignacion     _) = ":="

--
-- Impresion del AST (arbol sintactico abstracto)
--

data PrintState = PrintState {
    tabs :: Int
} deriving (Show)

initialPState :: PrintState
initialPState = PrintState {
    tabs = 0
}

type AST_String = DS.Seq String

correrImpresor :: Impresor () -> String
correrImpresor = (DF.foldl (++) "") . snd . runIdentity . runWriterT . (flip runStateT initialPState)

type Impresor a = StateT PrintState (WriterT AST_String Identity) a

impresor :: Inst -> Impresor ()

impresor (I_Assign id e) = do
    imprimirNoTerminal "ASIGNACION" 
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    imprimirExpresion    "- val: " e
    bajarTabs
impresor (I_If b exito) = do
    imprimirNoTerminal "CONDICIONAL" 
    subirTabs
    imprimirExpresion     "- guardia:" b
    imprimirInstrucciones "- exito: "  exito
    bajarTabs
impresor (I_IfElse b exito fallo) = do
    imprimirNoTerminal "CONDICIONAL IF-ELSE"
    subirTabs
    imprimirExpresion     "- guardia: " b
    imprimirInstrucciones "- exito: "   exito
    imprimirInstrucciones "- fallo: "   fallo
    bajarTabs
impresor (I_While b c) = do
    imprimirNoTerminal "ITERACION INDETERMINADA" 
    subirTabs
    imprimirExpresion     "- guardia: " b
    imprimirInstrucciones "- cuerpo: "  c
    bajarTabs
impresor (I_For id e1 e2 c) = do
    imprimirNoTerminal "ITERACION DETERMINADA - FOR" 
    subirTabs
    imprimirNoTerminal $  "- variable: " ++ id
    imprimirExpresion     "- lim_inferior: "     e1
    imprimirExpresion     "- lim_superior: "     e2
    imprimirInstrucciones "- cuerpo: " c
    bajarTabs
impresor (I_From e1 e2 c) = do
    imprimirNoTerminal "ITERACION DETERMINADA - FROM"
    subirTabs
    imprimirExpresion     "- lim_inferior: " e1 
    imprimirExpresion     "- lim_superior: " e2 
    imprimirInstrucciones "- cuerpo: "      c
    bajarTabs
impresor (I_Declare _ is) = imprimirInstrucciones "SECUENCIACION" is
impresor (I_Write e) = do
    imprimirNoTerminal "IMPRIMIR"
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresor (I_Read id) = do
    imprimirNoTerminal "LEER"
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    bajarTabs
impresor (I_Ejec cadena e) = do
    imprimirNoTerminal "EJECUCION"
    subirTabs
    imprimirCadena cadena
    imprimirExpresion "- cinta: " e
    bajarTabs
impresor (I_Concat e1 e2) = do
    imprimirNoTerminal "CONCATENACION"
    subirTabs
    imprimirExpresion "- 1era cadena: " e1
    imprimirExpresion "- 2da cadena: "  e2
    bajarTabs

--
--  Impresion de expresiones
--

impresorE :: Exp -> Impresor ()

impresorE (E_Const c)        = imprimirNoTerminal $ show c
impresorE (E_Var v)          = imprimirNoTerminal v
impresorE (E_True)           = imprimirNoTerminal "'True'"
impresorE (E_False)          = imprimirNoTerminal "'False'"
impresorE (E_BinOp op e1 e2) = do
    imprimirNoTerminal "BIN_ARITMETICO"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ (show op)
    imprimirExpresion    "- operador izquierdo: " e1
    imprimirExpresion    "- operador derecho: "   e2
    bajarTabs
impresorE (E_Comp op e1 e2)  = do
    imprimirNoTerminal "BIN_RELACIONAL"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ (show op)
    imprimirExpresion    "- operador izquierdo: " e1
    imprimirExpresion    "- operador derecho: "   e2
    bajarTabs
impresorE (E_UnOp op e) = do
    imprimirExpresion (show op) e
impresorE (E_Paren e) = do
    imprimirNoTerminal "PARENTESIS" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresorE (E_Corch e) = do
    imprimirNoTerminal "CORCHETES" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs

--
--  Funciones auxiliares para la impresion
--

subirTabs :: Impresor ()
subirTabs = modify (\s -> s { tabs = (tabs s) + 1 })

bajarTabs :: Impresor ()
bajarTabs = modify (\s -> s { tabs = (tabs s) - 1 })

imprimirNoTerminal :: String -> Impresor ()
imprimirNoTerminal str = do
    t <- gets tabs
    tell $ DS.singleton $ replicate t '\t' ++ str ++ "\n"

imprimirExpresion :: String -> Exp -> Impresor () 
imprimirExpresion tag e = do
    imprimirNoTerminal tag
    subirTabs
    impresorE e 
    bajarTabs

imprimirCadena :: [B_Inst] -> Impresor ()
imprimirCadena c = do
    imprimirNoTerminal "- cadena"
    subirTabs
    imprimirNoTerminal $ "{" ++ (concatMap show c) ++ "}" 
    bajarTabs

imprimirInstrucciones :: String -> [Inst] -> Impresor ()
imprimirInstrucciones tag is = do
    imprimirNoTerminal tag
    subirTabs
    mapM_ impresor is
    bajarTabs
}
