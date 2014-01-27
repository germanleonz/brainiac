{-|
  /Analizador de errores de contexto Brainiac/

   German Leon Z. <08-10611@ldc.usb.ve>
-}
module ContBrainiac (
    -- * Tipos exportados
    Analizador,
    BrainiacError (..),
    -- * Funciones exportadas
    correrAnalizador,
    analizar,
    continue,
    buscarTipo,
    buscarValor,
    buscarOcupada,
    cambiarValor,
    procesarDeclaraciones,
    eliminarDeclaraciones
)
where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Data.Char as C
import Data.Sequence as DS
import qualified Data.Map as DM

import Cinta
import TablaSim
import SinBrainiac

{-|
    Agregamos informacion de la variable id a la tabla de simbolos
-}
agregarSimbolo :: VarName -> InfoSim -> Analizador ()
agregarSimbolo id info = modify (\s -> s { tabla = insertar id info (tabla s)})

{-|
    Buscamos variable id en el scope mas interno esta bloqueada
    En caso de que no este en la tabla de simbolos se lanza un error
-}
buscarOcupada :: VarName -> Analizador Bool
buscarOcupada id = do
    tabla <- gets tabla
    maybe fallo exito $ buscarInfoSim id tabla 
    where exito = return . ocupado
          fallo = throwError $ VariableNoDeclarada id

{-|
    Buscamos el tipo de datos de la variable id en el scope mas interno.
    En caso de que no este en la tabla de simbolos se lanza un error
-}
buscarTipo :: VarName -> Analizador Tipo
buscarTipo id = do
    tabla <- gets tabla
    maybe fallo exito $ buscarInfoSim id tabla 
    where exito = return . tipo 
          fallo = throwError $ VariableNoDeclarada id

{-|
    Buscamos el valor de la variable id en el scope mas interno.
    En caso de que no este en la tabla de simbolos se lanza un error
-}
buscarValor :: VarName -> Analizador BrainVal
buscarValor id = do
    tabla <- gets tabla
    maybe fallo exito $ buscarInfoSim id tabla 
    where exito = return . valor
          fallo = throwError $ VariableNoDeclarada id

{-|
    Cambia el valor de la variable id por el valor vn en
    la tabla de simbolos
-}
cambiarValor :: VarName -> BrainVal -> Analizador ()
cambiarValor id vn = do
    estaOcupada <- buscarOcupada id
    if estaOcupada
        then throwError $ VariableDeIteracion id      
        else modify $ (\s -> s { tabla = actualizar id vn (tabla s) })

{-|
    Marcamos a la variable id como bloqueada en la tabla de simbolos
-}
marcarVariableOcupada :: VarName -> Analizador ()
marcarVariableOcupada id = do
    tabla <- gets tabla
    case buscarInfoSim id tabla of
        Just info -> do
            eliminarDeclaracion $ Decl id (tipo info) 
            agregarSimbolo id $ info { ocupado = True } 
        otherwise -> continue

{-|
    Marcamos a la variable id como libre en la tabla de simbolos
-}
marcarVariableLibre :: VarName -> Analizador ()
marcarVariableLibre id = do
    tabs <- gets tabla
    case buscarInfoSim id tabs of
        Just info -> do
            eliminarDeclaracion $ Decl id (tipo info) 
            agregarSimbolo id $ info { ocupado = False } 
        otherwise -> continue

{-|
    Agregamos a la tabla de simbolos las variables definidas en ds
    y aumentamos el scope actual
-}
procesarDeclaraciones :: [Declaracion] -> Analizador () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = (currentScope s) + 1 })
    mapM_ procesarDeclaracion ds

{-|
    Agregamos la variable v de tipo tn a la tabla de simbolos 
-}
procesarDeclaracion :: Declaracion -> Analizador ()
procesarDeclaracion (Decl id tn) = do
    tabla <- gets tabla
    cs    <- gets currentScope
    let info = InfoSim {
                 tipo    = tn,
                 scope   = cs,
                 valor   = Null,
                 ocupado = False
               }
    case buscarInfoSim id tabla of 
        Nothing                            -> agregarSimbolo id info
        Just (InfoSim _ l _ _) | l == cs   -> throwError $ MultiplesDeclaraciones id 
                               | otherwise -> agregarSimbolo id info

{-|
    Eliminamos de la tabla de simbolos las variables declaradas en ds
    y disminuimos en uno el scope actual
-}
eliminarDeclaraciones :: [Declaracion] -> Analizador ()
eliminarDeclaraciones ds = do
    mapM_ eliminarDeclaracion ds
    modify (\s -> s { currentScope = (currentScope s) - 1 })

{-|
    Eliminamos la declaracion d de la tabla de simbolos
-}
eliminarDeclaracion :: Declaracion -> Analizador ()
eliminarDeclaracion (Decl v _) = modify (\s -> s { tabla = eliminarVariable v (tabla s)})

{-|
    Definicion del Monad analizador de errores de contexto         
-}

type Analizador a = StateT EvalState (ErrorT BrainiacError IO) a 

correrAnalizador :: Analizador a -> IO (Either BrainiacError (a, EvalState))
correrAnalizador = runErrorT . (flip runStateT initialState)

data EvalState = EvalState {
    currentScope :: Scope,
    tabla        :: TablaSim
} deriving (Show)

initialState :: EvalState
initialState = EvalState {
    currentScope = -1,
    tabla        = tablaVacia
}

--
--  Errores de contexto y dinamicos
--

data BrainiacError = MultiplesDeclaraciones VarName
                   | VariableNoDeclarada VarName
                   | TiposNoCoinciden Exp Exp Tipo
                   | TipoIncorrecto Exp Tipo
                   | VariableDeIteracion VarName
                   | VariableNoInicializada VarName 
                   | ErrorLecturaCinta VarName
                   | DivisionPorCero Exp
                   | CintaMalFormada
                   | ErrorDeEntrada Tipo

instance Show BrainiacError where
    show (VariableNoDeclarada id)    = "Error estatico: " ++
        "La variable '" ++ id ++ "' no ha sido declarada"
    show (MultiplesDeclaraciones id) = "Error estatico: " ++
        "Redeclaracion de la variable '" ++ id ++ "'"
    show (TiposNoCoinciden e1 e2 t)  = "Error estatico: " ++
        "Los tipos de :\n" ++
        (show e1) ++ 
        (show e2) ++
        "Son incorrectos. Ambas expresiones deben ser de " ++ (show t)
    show (TipoIncorrecto e1 t)       = "Error estatico: " ++
        "La expresion: \n" ++
        (show e1) ++
        "Debe ser del tipo: " ++ (show t) 
    show (VariableDeIteracion id)    = "Error estatico: " ++
        "La variable: '" ++ id ++ "' no puede ser modificada en el ciclo"
    show (ErrorLecturaCinta id)      = "Error estatico: " ++ 
        "En la variable v '" ++ id ++ "'. No pueden leerse cintas " ++
        "de la entrada estandar"
    show (VariableNoInicializada id) = "Error estatico: " ++
        "La variable '" ++ id ++ "' no ha sido inicializada"
    show (DivisionPorCero e)  = "Error dinamico: " ++ 
        "division por cero en la expresion:\n" ++ (show e)
    show (CintaMalFormada)    = "Error dinamico: " ++
        " las cintas solo pueden tener tamano mayor a cero"
    show (ErrorDeEntrada t)   = "Error dinamico " ++ 
        "de lectura se esperaba un valor de " ++ (show t)

instance Error BrainiacError

analizar :: Inst -> Analizador () 

analizar (I_Declare ds is) = do
    procesarDeclaraciones ds
    analizarInstrucciones is
    eliminarDeclaraciones ds
analizar i@(I_Assign id exp) = do
    estaOcupada <- buscarOcupada id
    if estaOcupada 
        then throwError $ VariableDeIteracion id
        else continue
    t_var <- buscarTipo id
    case t_var of 
        Tipo_Tape -> do
            case exp of
                (E_Corch ec) -> chequearTipoDeExpresion     ec  Tipo_Integer
                otherwise    -> throwError $ TipoIncorrecto exp Tipo_Integer
            continue
        otherwise -> do
            chequearTipoDeExpresion exp t_var
            continue
analizar i@(I_If cond exito) = do
    chequearTipoDeExpresion cond Tipo_Boolean
    analizarInstrucciones exito
analizar i@(I_IfElse cond exito fallo) = do
    chequearTipoDeExpresion cond Tipo_Boolean
    analizarInstrucciones exito
    analizarInstrucciones fallo
analizar i@(I_While guardia is) = do
    chequearTipoDeExpresion guardia Tipo_Boolean
    analizarInstrucciones is 
analizar i@(I_For id e1 e2 is) = do
    estaOcupada <- buscarOcupada id
    if estaOcupada 
        then throwError $ VariableDeIteracion id
        else continue
    t <- buscarTipo id
    if t == Tipo_Integer 
        then continue
        else throwError $ TipoIncorrecto (E_Var id) Tipo_Integer
    chequearTipoDeExpresion e1 Tipo_Integer
    chequearTipoDeExpresion e2 Tipo_Integer
    marcarVariableOcupada id
    analizarInstrucciones is
    marcarVariableLibre id
analizar i@(I_From e1 e2 is) = do
    chequearTipoDeExpresion e1 Tipo_Integer
    chequearTipoDeExpresion e2 Tipo_Integer
    analizarInstrucciones is
analizar i@(I_Write e) = conseguirTipo e >> continue 
analizar i@(I_Read id) = do
    t <- buscarTipo id 
    case t of
        Tipo_Tape -> throwError $ ErrorLecturaCinta id
        otherwise -> continue
analizar i@(I_Ejec cadena e) = do
    case e of
        (E_Var _)    -> chequearTipoDeExpresion e  Tipo_Tape
        (E_Corch ec) -> chequearTipoDeExpresion ec Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e Tipo_Tape
    continue
analizar i@(I_Concat e1 e2) = do
    case e1 of
        (E_Corch ec) -> chequearTipoDeExpresion e1 Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e1 Tipo_Integer
    case e2 of
        (E_Var _)    -> chequearTipoDeExpresion e2 Tipo_Tape
        otherwise    -> throwError $ TipoIncorrecto e1 Tipo_Tape
    continue

analizarInstrucciones :: [Inst] -> Analizador ()
analizarInstrucciones = mapM_ analizar

conseguirTipo :: Exp -> Analizador Tipo

conseguirTipo (E_Const _)        = return Tipo_Integer
conseguirTipo (E_Var id)         = buscarTipo id 
conseguirTipo (E_True)           = return Tipo_Boolean
conseguirTipo (E_False)          = return Tipo_Boolean
conseguirTipo (E_BinOp op e1 e2) = do
    case op of 
        Op_Con    -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean
        Op_Dis    -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean
        otherwise -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
conseguirTipo (E_Comp op e1 e2)  = do
    t1 <- conseguirTipo e1 
    t2 <- conseguirTipo e2
    case op of
        Op_Eq     -> if ((t1 == Tipo_Boolean) && (t2 == Tipo_Boolean)) ||
                        ((t1 == Tipo_Integer) && (t2 == Tipo_Integer)) 
                        then return Tipo_Boolean
                        else throwError $ TiposNoCoinciden e1 e2 Tipo_Boolean
        Op_Neq    -> if ((t1 == Tipo_Boolean) && (t2 == Tipo_Boolean)) ||
                        ((t1 == Tipo_Integer) && (t2 == Tipo_Integer)) 
                        then return Tipo_Boolean
                        else throwError $ TiposNoCoinciden e1 e2 Tipo_Boolean
        otherwise -> if (t1 == Tipo_Integer) && (t2 == Tipo_Integer)
                        then return Tipo_Boolean
                        else throwError $ TiposNoCoinciden e1 e2 Tipo_Integer
conseguirTipo (E_UnOp op e) = do
    case op of
        Op_NegArit -> chequearTipoDeExpresion e Tipo_Integer
        Op_NegBool -> chequearTipoDeExpresion e Tipo_Boolean
        Op_Inspecc -> do 
            chequearTipoDeExpresion e Tipo_Tape
            return Tipo_Integer
conseguirTipo (E_Paren e) = conseguirTipo e
conseguirTipo (E_Corch e) = conseguirTipo e

chequearTipoDeExpresiones :: Exp -> Exp -> Tipo -> Analizador Tipo
chequearTipoDeExpresiones e1 e2 t = do
    t1 <- conseguirTipo e1
    t2 <- conseguirTipo e2
    if (t1 == t) && (t2 == t)
        then return t
        else throwError $ TiposNoCoinciden e1 e2 t

chequearTipoDeExpresion :: Exp -> Tipo -> Analizador Tipo
chequearTipoDeExpresion e t = do
    t1 <- conseguirTipo e
    if t1 == t
        then return t
        else throwError $ TipoIncorrecto e t

continue :: Analizador ()
continue = return ()
