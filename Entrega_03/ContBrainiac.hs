{-
   Entrega 03 - Analisis de contexto

   Hecho por: German Leon Z. Carnet: 08-10611
   
 -}
module ContBrainiac (
    Analizador,
    correrAnalizador,
    analizar,
    getType
)
where

import SinBrainiac

import qualified Data.Map as DM
import Data.Sequence as DS
 
import Control.Monad.Error
import Control.Monad.State

--
--  Tabla de simbolos basada en tabla de hash con Strings como claves y Data.Sequence 
--  de SymInfo como valores
--

data SymTable = SymTable (DM.Map VarName (Seq SymInfo))
    deriving (Show)

data SymInfo = SymInfo Tipo Scope Valor Bool
    deriving (Show)

type Scope = Int

{-| 
  Crear una tabla de simbolos vacia    
-}
tablaVacia :: SymTable 
tablaVacia = SymTable (DM.empty)

{-|
  Agregar la variable v a la tabla de simbolos
-}
insertar :: VarName    -- ^ Simbolo que se va a agregar
         -> SymInfo    -- ^ Tipo Valor y scope donde esta el simbolo
         -> SymTable   -- ^ La tabla de simbolos donde se va a agregar el simbolo
         -> SymTable   -- ^ La tabla de simbolos con el elemento incluido
insertar vn info (SymTable m)  =  SymTable $ DM.alter f vn m
    where f Nothing  = Just (DS.singleton info)
          f (Just x) = Just (info <| x)

{-|
    Eliminar la variable v del scope mas interno
-}
eliminarVariable :: VarName 
                 -> SymTable
                 -> SymTable
eliminarVariable v (SymTable m) = SymTable $ DM.alter f v m
    where f Nothing  = Just DS.empty
          f (Just s) = Just $ DS.drop 1 s

{-|
    Buscar la informacion relacionada con una variable en la tabla de simbolos
-}
buscarSymInfo :: VarName
              -> SymTable 
              -> Maybe SymInfo 
buscarSymInfo str (SymTable m) =
    case DM.lookup str m of
        Nothing -> Nothing
        Just s -> case viewl s of
            EmptyL     -> Nothing
            info :< xs -> Just info

{-|
    La variable id se bloquea por ser variable de iteracion
-}
{-bloquearVariable :: VarName -}
                 {--> SymTable-}
                 {--> SymTable -}
{-bloquearVariable id (SymTable m) = do-}
    {-case buscarSymInfo id tabla of-}
        {-Just (SymInfo t _ _ _) -> Just (SymInfo t _ -}
        {-Nothing                -> Nothing-}

{-|
    La variable id se libera por dejar de ser variable de iteracion
-}
{-liberarVariable :: VarName-}
                {--> SymTable-}
                {--> SymTable-}
{-liberarVariable id (SymTable m) = undefined-}

--
--  Funciones para manipular la tabla de simbolos dentro del Analizador
--
{-|
    Agregamos informacion de la variable id a la tabla de simbolos
-}
agregarSimbolo :: VarName
               -> SymInfo
               -> Analizador ()
agregarSimbolo id info = modify (\s -> s { tabla = insertar id info (tabla s)})

{-|
    Buscamos el tipo de datos de la variable id en el scope mas cercano.
    En caso de que no este en la tabla de simbolos se lanza un error
-}
buscarTipo :: VarName
           -> Analizador Tipo
buscarTipo id = do
    tabla <- gets tabla
    case buscarSymInfo id tabla of
        Just (SymInfo t _ _ _) -> return t
        Nothing                -> throwError $ VariableNoDeclarada id

{-|
    Marcamos a la variable id como bloqueada en la tabla de simbolos
-}
marcarVariableOcupada :: VarName 
                      -> Analizador ()
marcarVariableOcupada id = do
    tabs <- gets tabla
    case buscarSymInfo id tabs of
        Just (SymInfo t l val b) -> do
            eliminarDeclaracion (Decl id t) 
            agregarSimbolo id (SymInfo t l val True)
        otherwise -> return ()

{-|
    Marcamos a la variable id como bloqueada en la tabla de simbolos
-}
marcarVariableLibre :: VarName 
                    -> Analizador ()
marcarVariableLibre id = do
    tabs <- gets tabla
    case buscarSymInfo id tabs of
        Just (SymInfo t l val b) -> do
            eliminarDeclaracion (Decl id t) 
            agregarSimbolo id (SymInfo t l val False)
        otherwise -> return ()

{-|
    Eliminamos la declaracion d de la tabla de simbolos
-}
eliminarDeclaracion :: Declaracion 
                    -> Analizador ()
eliminarDeclaracion (Decl v _) = modify (\s -> s { tabla = eliminarVariable v (tabla s)})

{-|
    Agregamos la variable v de tipo tn a la tabla de simbolos 
-}
procesarDeclaracion :: Declaracion
                    -> Analizador ()
procesarDeclaracion (Decl v tn) = do
    st <- get 
    case buscarSymInfo v (tabla st) of 
        Nothing                  -> agregarSimbolo v (SymInfo tn (currentScope st) (-1) False)
        Just (SymInfo t l val b) -> if l == (currentScope st)
                                    then throwError $ MultiplesDeclaraciones v 
                                    else agregarSimbolo v (SymInfo tn (currentScope st) (-1) False)

{-|
    Agregamos a la tabla de simbolos las variables definidas en ds
    y aumentamos el scope actual
-}
procesarDeclaraciones :: [Declaracion]
                      -> Analizador () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = (currentScope s) + 1 })
    mapM_ procesarDeclaracion ds
            
{-|
    Eliminamos de la tabla de simbolos las variables declaradas en ds
    y disminuimos en uno el scope actual
-}
eliminarDeclaraciones :: [Declaracion]
                      -> Analizador ()
eliminarDeclaraciones ds = do
    mapM_ eliminarDeclaracion ds
    modify (\s -> s { currentScope = (currentScope s) - 1 })

--
--  Definicion del Monad analizador de errores de contexto
--

type Analizador a = StateT EvalState (ErrorT ContextError IO) a

correrAnalizador :: Analizador a -> IO (Either ContextError (a, EvalState))
correrAnalizador = runErrorT . (flip runStateT initialState)

data EvalState = EvalState {
    currentScope :: Scope,
    tabla        :: SymTable 
} deriving (Show)

initialState :: EvalState
initialState = EvalState {
    currentScope = 0,
    tabla        = tablaVacia
}

--
--  Errores de contexto
--
data ContextError = MultiplesDeclaraciones VarName
                  | VariableNoDeclarada VarName
                  | VariableNoInicializada VarName
                  | TiposNoCoinciden Exp Exp Tipo
                  | TipoIncorrecto Exp Tipo
                  | VariableDeIteracion VarName

instance Show ContextError where
    show (MultiplesDeclaraciones v) = "Error estatico: " ++
        "Multiples declaraciones de la variable '" ++ v ++ "'"
    show (VariableNoDeclarada v)    = "Error estatico: " ++
        "La variable '" ++ v ++ "' no ha sido declarada"
    show (VariableNoInicializada v) = "Error estatico: " ++
        "La variable '" ++ v ++ "' no ha sido inicializada"
    show (TiposNoCoinciden e1 e2 t) = "Los tipos de :\n" ++
        (show e1) ++ 
        (show e2) ++
        "Son incorrectos. Ambas expresiones deben ser de " ++ (show t)
    show (TipoIncorrecto e1 t) = "La expresion: \n" ++
        (show e1) ++
        "Debe ser del tipo: " ++ (show t) 
    show (VariableDeIteracion id) = "La variable: '" ++ id ++
        "' es una variable protegida, no puede modificarse dentro de un ciclo"

instance Error ContextError

analizar :: Inst -> Analizador () 

analizar (I_Declare ds is) = do
    procesarDeclaraciones ds
    analizarInstrucciones is
    eliminarDeclaraciones ds
analizar (I_Assign id exp) = do
    t_var  <- buscarTipo id
    tabs   <- gets tabla
    case buscarSymInfo id tabs of
        Just (SymInfo _ _ _ True) -> throwError $ VariableDeIteracion id
        otherwise                 -> return ()
    case t_var of 
        --  Si id es una variable cinta entonces el lado
        --  derecho debe ser una expresion de la forma [ E ] 
        --  donde E es de tipo entero u otra variable tipo cinta
        Tipo_Tape -> do
            case exp of
                (E_Var _)    -> chequearTipoDeExpresion exp Tipo_Tape
                (E_Corch ec) -> chequearTipoDeExpresion ec Tipo_Integer
                otherwise    -> throwError $ TipoIncorrecto exp Tipo_Tape
            return ()
        --  En caso contrario se verifica que ambos lados de la asignacion
        --  sean del mismo tipo 
        otherwise -> do
            chequearTipoDeExpresion exp t_var
            return ()
analizar (I_If cond exito) = do
    chequearTipoDeExpresion cond Tipo_Boolean
    analizarInstrucciones exito
analizar (I_IfElse cond exito fallo) = do
    chequearTipoDeExpresion cond Tipo_Boolean
    analizarInstrucciones exito
    analizarInstrucciones fallo
analizar (I_While guardia is) = do
    chequearTipoDeExpresion guardia Tipo_Boolean
    analizarInstrucciones is 
analizar (I_For id e1 e2 is) = do
    t <- buscarTipo id
    chequearTipoDeExpresion (E_Var id) Tipo_Integer
    chequearTipoDeExpresion e1 Tipo_Integer
    chequearTipoDeExpresion e2 Tipo_Integer
    marcarVariableOcupada id
    analizarInstrucciones is
    marcarVariableLibre id
analizar (I_From e1 e2 is) = do
    chequearTipoDeExpresion e1 Tipo_Integer
    chequearTipoDeExpresion e2 Tipo_Integer
    analizarInstrucciones is
analizar (I_Write e)  = return ()
analizar (I_Read id ) = do
    buscarTipo id 
    return ()
analizar (I_Ejec cadena e) = do
    chequearTipoDeExpresion e Tipo_Tape
    return ()
analizar (I_Concat e1 e2) = do
    case e1 of
        (E_Var _)    -> chequearTipoDeExpresion e1 Tipo_Tape
        (E_Corch ec) -> chequearTipoDeExpresion e1 Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e1 Tipo_Tape
    case e2 of
        (E_Var _)    -> chequearTipoDeExpresion e2 Tipo_Tape
        (E_Corch ec) -> chequearTipoDeExpresion ec Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e1 Tipo_Tape
    return ()

getType :: Exp -> Analizador Tipo

getType (E_Const _)          = return Tipo_Integer
getType (E_Var id)           = buscarTipo id 
getType (E_True)             = return Tipo_Boolean
getType (E_False)            = return Tipo_Boolean
getType (E_BinOp op e1 e2)   = do
    case op of 
        Op_Con    -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean
        Op_Dis    -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean
        otherwise -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
getType (E_Comp op e1 e2)  = do
    chequearTipoDeExpresiones e1 e2 Tipo_Integer
    return Tipo_Boolean
getType (E_UnOp op e) = do
    case op of
        Op_NegArit -> chequearTipoDeExpresion e Tipo_Integer
        Op_NegBool -> chequearTipoDeExpresion e Tipo_Boolean
        Op_Inspecc -> do 
            chequearTipoDeExpresion e Tipo_Tape
            return Tipo_Integer
getType (E_Paren e) = getType e
getType (E_Corch e) = getType e

analizarInstrucciones :: [Inst]
                      -> Analizador ()
analizarInstrucciones = mapM_ analizar

chequearTipoDeExpresiones :: Exp 
                          -> Exp
                          -> Tipo
                          -> Analizador Tipo
chequearTipoDeExpresiones e1 e2 t = do
    t1 <- getType e1
    t2 <- getType e2
    if (t1 == t) && (t2 == t)
        then return t
        else throwError $ TiposNoCoinciden e1 e2 t

chequearTipoDeExpresion :: Exp
                        -> Tipo
                        -> Analizador Tipo
chequearTipoDeExpresion e t = do
    t1 <- getType e
    if t1 == t
        then return t
        else throwError $ TipoIncorrecto e t
