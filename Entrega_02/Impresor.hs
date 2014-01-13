module Impresor (
    Impresor,
    correrImpresor,
    impresor
) where

import SinBrainiac
import Control.Monad.State

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

correrImpresor :: Impresor () -> IO ((), PrintState) 
correrImpresor = (flip runStateT) initialPState 

type Impresor a = StateT PrintState IO a

impresor :: Inst -> Impresor ()
impresor (I_Assign id e) = do
    imprimirNoTerminal "ASIGNACION" 
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    imprimirExpresion "- val: " e
    bajarTabs
impresor (I_If b exito) = do
    imprimirNoTerminal "CONDICIONAL" 
    subirTabs
    imprimirBooleano "- guardia:" b
    imprimirInstrucciones "- exito: " exito
    bajarTabs
impresor (I_IfElse b exito fallo) = do
    imprimirNoTerminal "CONDICIONAL_IF_ELSE"
    subirTabs
    imprimirBooleano "- guardia: " b
    imprimirInstrucciones "- exito: " exito
    imprimirInstrucciones "- fallo: " fallo
    bajarTabs
impresor (I_While b c) = do
    imprimirNoTerminal "ITERACION_INDETERMINADA" 
    subirTabs
    imprimirBooleano "- guardia:" b
    imprimirInstrucciones "- cuerpo:" c
    bajarTabs
impresor (I_For id e1 e2 c) = do
    imprimirNoTerminal "ITERACION_DETERMINADA - FOR" 
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    imprimirExpresion "- e1: " e1
    imprimirExpresion "- e2: " e2
    imprimirInstrucciones "- cuerpo: " c
    bajarTabs
impresor (I_From e1 e2 c) = do
    imprimirNoTerminal "ITERACION_DETERMINADA - FROM"
    subirTabs
    imprimirExpresion "- lim_inferior:" e1 
    imprimirExpresion "- lim_superior:" e2 
    imprimirInstrucciones "- cuerpo: " c
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
    imprimirExpresion "- operador izquierdo: " e1
    imprimirExpresion "- operador derecho: " e2
    bajarTabs
impresorE (E_UnOp op e) = do
    imprimirExpresion (show op) e
impresorE (E_Paren e)   = do
    imprimirNoTerminal "PARENTESIS" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresorE (E_Corch e)   = do
    imprimirNoTerminal "CORCHETES" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs

impresorB :: BoolExp -> Impresor ()
impresorB (B_True)           = imprimirNoTerminal "'True'"
impresorB (B_False)          = imprimirNoTerminal "'False'"
impresorB (B_Comp op e1 e2)  = do
    imprimirNoTerminal "BIN_RELACIONAL"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ (show op)
    imprimirExpresion "- operador izquierdo: " e1
    imprimirExpresion "- operador derecho: " e2
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
    liftIO $ putStrLn $ replicate t '\t' ++ str

imprimirExpresion :: String -> Exp -> Impresor () 
imprimirExpresion tag e = do
    imprimirNoTerminal tag
    subirTabs
    impresorE e 
    bajarTabs

imprimirCadena :: [B_Inst] -> Impresor ()
imprimirCadena c = mapM_ (imprimirNoTerminal . show) c

imprimirInstrucciones :: String -> [Inst] -> Impresor ()
imprimirInstrucciones tag is = do
    imprimirNoTerminal tag
    subirTabs
    mapM_ impresor is
    bajarTabs

imprimirBooleano :: String -> BoolExp -> Impresor ()
imprimirBooleano tag b = do
    imprimirNoTerminal tag
    subirTabs
    impresorB b 
    bajarTabs 
