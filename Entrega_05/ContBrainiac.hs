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

import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char as C
import qualified Data.Foldable as DF
import qualified Data.Map as DM
import           Data.Sequence as DS

import Cinta
import TablaSim
import Language

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
procesarDeclaraciones :: Seq Declaracion -> Analizador () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = currentScope s + 1 })
    DF.mapM_ procesarDeclaracion ds

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
eliminarDeclaraciones :: Seq Declaracion -> Analizador ()
eliminarDeclaraciones ds = do
    DF.mapM_ eliminarDeclaracion ds
    modify (\s -> s { currentScope = currentScope s - 1 })

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
correrAnalizador = runErrorT . flip runStateT initialState

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

analizar (IDeclare ds is) = do
    procesarDeclaraciones ds
    analizarInstrucciones is
    eliminarDeclaraciones ds
analizar i@(IAssign id exp) = do
    estaOcupada <- buscarOcupada id
    if estaOcupada 
        then throwError $ VariableDeIteracion id
        else continue
    t_var <- buscarTipo id
    case t_var of 
        TipoTape -> do
            case exp of
                (ECorch ec) -> chequearTipoDeExpresion     ec  TipoInteger
                otherwise    -> throwError $ TipoIncorrecto exp TipoInteger
            continue
        otherwise -> do
            chequearTipoDeExpresion exp t_var
            continue
analizar i@(IIf cond exito) = do
    chequearTipoDeExpresion cond TipoBoolean
    analizarInstrucciones exito
analizar i@(IIfElse cond exito fallo) = do
    chequearTipoDeExpresion cond TipoBoolean
    analizarInstrucciones exito
    analizarInstrucciones fallo
analizar i@(IWhile guardia is) = do
    chequearTipoDeExpresion guardia TipoBoolean
    analizarInstrucciones is 
analizar i@(IFor id e1 e2 is) = do
    estaOcupada <- buscarOcupada id
    if estaOcupada 
        then throwError $ VariableDeIteracion id
        else continue
    t <- buscarTipo id
    if t == TipoInteger 
        then continue
        else throwError $ TipoIncorrecto (EVar id) TipoInteger
    chequearTipoDeExpresion e1 TipoInteger
    chequearTipoDeExpresion e2 TipoInteger
    marcarVariableOcupada id
    analizarInstrucciones is
    marcarVariableLibre id
analizar i@(IFrom e1 e2 is) = do
    chequearTipoDeExpresion e1 TipoInteger
    chequearTipoDeExpresion e2 TipoInteger
    analizarInstrucciones is
analizar i@(IWrite e) = conseguirTipo e >> continue 
analizar i@(IRead id) = do
    t <- buscarTipo id 
    case t of
        TipoTape -> throwError $ ErrorLecturaCinta id
        otherwise -> continue
analizar i@(IEjec cadena e) = do
    case e of
        (EVar _)    -> chequearTipoDeExpresion e  TipoTape
        (ECorch ec) -> chequearTipoDeExpresion ec TipoInteger
        otherwise    -> throwError $ TipoIncorrecto e TipoTape
    continue
analizar i@(IConcat e1 e2) = do
    case e1 of
        (ECorch ec) -> chequearTipoDeExpresion e1 TipoInteger
        otherwise    -> throwError $ TipoIncorrecto e1 TipoInteger
    case e2 of
        (EVar _)    -> chequearTipoDeExpresion e2 TipoTape
        otherwise    -> throwError $ TipoIncorrecto e1 TipoTape
    continue

analizarInstrucciones :: Seq Inst -> Analizador ()
analizarInstrucciones = DF.mapM_ analizar

conseguirTipo :: Exp -> Analizador Tipo

conseguirTipo (EConst _)        = return TipoInteger
conseguirTipo (EVar id)         = buscarTipo id 
conseguirTipo (ETrue)           = return TipoBoolean
conseguirTipo (EFalse)          = return TipoBoolean
conseguirTipo (EBinOp op e1 e2) = do
    case op of 
        OpCon    -> chequearTipoDeExpresiones e1 e2 TipoBoolean
        OpDis    -> chequearTipoDeExpresiones e1 e2 TipoBoolean
        otherwise -> chequearTipoDeExpresiones e1 e2 TipoInteger
conseguirTipo (EComp op e1 e2)  = do
    t1 <- conseguirTipo e1 
    t2 <- conseguirTipo e2
    case op of
        OpEq     -> if ((t1 == TipoBoolean) && (t2 == TipoBoolean)) ||
                        ((t1 == TipoInteger) && (t2 == TipoInteger)) 
                        then return TipoBoolean
                        else throwError $ TiposNoCoinciden e1 e2 TipoBoolean
        OpNeq    -> if ((t1 == TipoBoolean) && (t2 == TipoBoolean)) ||
                        ((t1 == TipoInteger) && (t2 == TipoInteger)) 
                        then return TipoBoolean
                        else throwError $ TiposNoCoinciden e1 e2 TipoBoolean
        otherwise -> if (t1 == TipoInteger) && (t2 == TipoInteger)
                        then return TipoBoolean
                        else throwError $ TiposNoCoinciden e1 e2 TipoInteger
conseguirTipo (EUnOp op e) = do
    case op of
        OpNegArit -> chequearTipoDeExpresion e TipoInteger
        OpNegBool -> chequearTipoDeExpresion e TipoBoolean
        OpInspecc -> do 
            chequearTipoDeExpresion e TipoTape
            return TipoInteger
conseguirTipo (EParen e) = conseguirTipo e
conseguirTipo (ECorch e) = conseguirTipo e

chequearTipoDeExpresiones :: Exp -> Exp -> Tipo -> Analizador Tipo
chequearTipoDeExpresiones e1 e2 t = do
    t1 <- conseguirTipo e1
    t2 <- conseguirTipo e2
    if (t1 == t) && (t2 == t)
        then return t -- ExpBinary op e1 e2 Bool
        else throwError $ TiposNoCoinciden e1 e2 t

chequearTipoDeExpresion :: Exp -> Tipo -> Analizador Tipo
chequearTipoDeExpresion e t = do
    t1 <- conseguirTipo e
    if t1 == t
        then return t
        else throwError $ TipoIncorrecto e t

continue :: Analizador ()
continue = return ()
