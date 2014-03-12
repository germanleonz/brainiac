module Language where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Foldable as DF
import           Data.Sequence as DS hiding (replicate)
--
-- Tipos de datos que representan un programa en Brainiac
--

type VarName = String
type Valor   = Int

data Declaracion = Decl VarName Tipo deriving (Show)

data Inst = IAssign VarName Exp
          | IIf     Exp (Seq Inst) 
          | IIfElse Exp (Seq Inst) (Seq Inst)
          | IWhile  Exp (Seq Inst)
          | IFor    VarName Exp Exp (Seq Inst)
          | IFrom   Exp Exp (Seq Inst)
          | IDeclare (Seq Declaracion) (Seq Inst)
          | IWrite Exp
          | IRead VarName
          | IEjec (Seq BInst) Exp
          | IConcat Exp Exp

instance Show Inst where show = correrImpresor . impresor 

data Exp = EConst Valor 
         | EVar   VarName 
         | ETrue 
         | EFalse 
         | EBinOp OpBin  Exp Exp 
         | EComp  OpComp Exp Exp 
         | EUnOp  OpUn   Exp 
         | EParen Exp 
         | ECorch Exp

instance Show Exp where show = correrImpresor . impresorE

data BInst = CSum
           | CRes
           | CIzq
           | CDer
           | CImp
           | CLee

instance Show BInst where
    show (CSum) = "+"
    show (CRes) = "-"
    show (CIzq) = "<"
    show (CDer) = ">"
    show (CImp) = "."
    show (CLee) = ","

data OpBin = OpSum 
           | OpRes
           | OpMul 
           | OpDiv 
           | OpMod
           | OpCon
           | OpDis

instance Show OpBin where
    show OpSum = "'Suma'"
    show OpRes = "'Resta'"
    show OpMul = "'Multiplicacion'"
    show OpDiv = "'Division'"
    show OpMod = "'Modulo'"
    show OpDis = "'Disyuncion'"
    show OpCon = "'Conjuncion'"

data OpComp = OpEq
            | OpNeq
            | OpLt 
            | OpLeq 
            | OpGt
            | OpGeq 

instance Show OpComp where
    show OpEq  = "'Igual'"
    show OpNeq = "'No Igual'"
    show OpLt  = "'Menor que'"
    show OpLeq = "'Menor o igual'"
    show OpGt  = "'Mayor que'"
    show OpGeq = "'Mayor o igual'"

data OpUn = OpNegArit
          | OpNegBool
          | OpInspecc

instance Show OpUn where
    show OpNegArit = "'Negacion Aritmetica'"
    show OpNegBool = "'Negacion Booleana'"
    show OpInspecc = "'Inspeccion'"

data Tipo = TipoBoolean
          | TipoInteger 
          | TipoTape
          deriving (Eq)

instance Show Tipo where
    show (TipoInteger) = "tipo entero"
    show (TipoBoolean) = "tipo boolean"
    show (TipoTape)    = "tipo cinta"

--
-- Impresion del AST (Arbol Sintactico Abstracto)
--

data PrintState = PrintState {
    tabs :: Int
} deriving (Show)

initialPState :: PrintState
initialPState = PrintState {
    tabs = 0
}

type ASTString = DS.Seq String

correrImpresor :: Impresor () -> String
correrImpresor = (DF.foldr (++) "") . snd . runIdentity . runWriterT . (flip runStateT initialPState)

type Impresor a = StateT PrintState (WriterT ASTString Identity) a

impresor :: Inst -> Impresor ()

impresor (IAssign id e) = do
    imprimirNoTerminal "ASIGNACION" 
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    imprimirExpresion    "- val: " e
    bajarTabs
impresor (IIf b exito) = do
    imprimirNoTerminal "CONDICIONAL" 
    subirTabs
    imprimirExpresion     "- guardia:" b
    imprimirInstrucciones "- exito: "  exito
    bajarTabs
impresor (IIfElse b exito fallo) = do
    imprimirNoTerminal "CONDICIONAL IF-ELSE"
    subirTabs
    imprimirExpresion     "- guardia: " b
    imprimirInstrucciones "- exito: "   exito
    imprimirInstrucciones "- fallo: "   fallo
    bajarTabs
impresor (IWhile b c) = do
    imprimirNoTerminal "ITERACION INDETERMINADA" 
    subirTabs
    imprimirExpresion     "- guardia: " b
    imprimirInstrucciones "- cuerpo: "  c
    bajarTabs
impresor (IFor id e1 e2 c) = do
    imprimirNoTerminal "ITERACION DETERMINADA - FOR" 
    subirTabs
    imprimirNoTerminal $  "- variable: " ++ id
    imprimirExpresion     "- lim_inferior: "     e1
    imprimirExpresion     "- lim_superior: "     e2
    imprimirInstrucciones "- cuerpo: " c
    bajarTabs
impresor (IFrom e1 e2 c) = do
    imprimirNoTerminal "ITERACION DETERMINADA - FROM"
    subirTabs
    imprimirExpresion     "- lim_inferior: " e1 
    imprimirExpresion     "- lim_superior: " e2 
    imprimirInstrucciones "- cuerpo: "      c
    bajarTabs
impresor (IDeclare _ is) = imprimirInstrucciones "SECUENCIACION" is
impresor (IWrite e) = do
    imprimirNoTerminal "IMPRIMIR"
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresor (IRead id) = do
    imprimirNoTerminal "LEER"
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    bajarTabs
impresor (IEjec cadena e) = do
    imprimirNoTerminal "EJECUCION"
    subirTabs
    imprimirCadena cadena
    imprimirExpresion "- cinta: " e
    bajarTabs
impresor (IConcat e1 e2) = do
    imprimirNoTerminal "CONCATENACION"
    subirTabs
    imprimirExpresion "- 1era cadena: " e1
    imprimirExpresion "- 2da cadena: "  e2
    bajarTabs

--
--  Impresion de expresiones
--

impresorE :: Exp -> Impresor ()

impresorE (EConst c)        = imprimirNoTerminal $ show c
impresorE (EVar v)          = imprimirNoTerminal v
impresorE (ETrue)           = imprimirNoTerminal "'True'"
impresorE (EFalse)          = imprimirNoTerminal "'False'"
impresorE (EBinOp op e1 e2) = do
    imprimirNoTerminal "BIN_ARITMETICO"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ show op
    imprimirExpresion    "- operador izquierdo: " e1
    imprimirExpresion    "- operador derecho: "   e2
    bajarTabs
impresorE (EComp op e1 e2)  = do
    imprimirNoTerminal "BIN_RELACIONAL"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ show op
    imprimirExpresion    "- operador izquierdo: " e1
    imprimirExpresion    "- operador derecho: "   e2
    bajarTabs
impresorE (EUnOp op e) = imprimirExpresion (show op) e
impresorE (EParen e) = do
    imprimirNoTerminal "PARENTESIS" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresorE (ECorch e) = do
    imprimirNoTerminal "CORCHETES" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs

--
--  Funciones auxiliares para la impresion
--

subirTabs :: Impresor ()
subirTabs = modify (\s -> s { tabs = tabs s + 1 })

bajarTabs :: Impresor ()
bajarTabs = modify (\s -> s { tabs = tabs s - 1 })

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

imprimirCadena :: Seq BInst -> Impresor ()
imprimirCadena c = do
    imprimirNoTerminal "- cadena"
    subirTabs
    imprimirNoTerminal $ '{' : show (DF.concatMap show c) ++ "}" 
    bajarTabs

imprimirInstrucciones :: String -> Seq Inst -> Impresor ()
imprimirInstrucciones tag is = do
    imprimirNoTerminal tag
    subirTabs
    DF.mapM_ impresor is
    bajarTabs
