{-
 -  Entrega 03 - Analisis de contexto
 -}
module ContBrainiac (
    Analizador,
    runAnalizador,
    analizarI,
    getType
)
where

import SinBrainiac

import qualified Data.Map as DM
import Data.Sequence as DS
 
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

--
--  Tabla de simbolos basada en tabla de hash con Strings como claves y Data.Sequence 
--  de SymInfo como valores
--

data SymTable = SymTable (DM.Map VarName (Seq SymInfo))
    deriving (Show)

data SymInfo = SymInfo Tipo Scope Valor
    deriving (Show)

type Scope = Int

{-| 
  Crear una tabla de simbolos vacia    
-}
tablaVacia :: SymTable 
tablaVacia = SymTable (DM.empty)

{-|
  Agregar una nueva variable a la tabla de simbolos
-}
insertar :: VarName    -- ^ Simbolo que se va a agregar
         -> SymInfo    -- ^ Tipo Valor y scope donde esta el simbolo
         -> SymTable   -- ^ La tabla de simbolos donde se va a agregar el simbolo
         -> SymTable   -- ^ La tabla de simbolos con el elemento incluido
insertar vn info (SymTable m)  =  SymTable $ DM.alter f vn m
    where f Nothing  = Just (DS.singleton info)
          f (Just x) = Just (info <| x)

{-|
    Eliminar una variable del scope especificado
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

--
--  Funciones para manipular la tabla de simbolos dentro del Analizador
--
agregarSimbolo :: VarName
               -> SymInfo
               -> Analizador ()
agregarSimbolo str info = do
    modify (\s -> s { tabla = insertar str info (tabla s)})

buscarTipo :: VarName
           -> Analizador Tipo
buscarTipo id = do
    tabla <- gets tabla
    case buscarSymInfo id tabla of
        Just (SymInfo t _ _) -> return t
        Nothing              -> throwError $ VariableNoDeclarada id

eliminarDeclaracion :: Declaracion 
                    -> Analizador ()
eliminarDeclaracion (Decl v t) = do
    modify (\s -> s { tabla = eliminarVariable v (tabla s)})

procesarDeclaracion :: Declaracion
                    -> Analizador ()
procesarDeclaracion (Decl v tn) = do
    st <- get 
    case buscarSymInfo v (tabla st) of 
        Nothing                -> agregarSimbolo v (SymInfo tn (currentScope st) (-1))
        Just (SymInfo t l val) -> if (l == (currentScope st)) && (tn == t)
                                  then throwError $ MultiplesDeclaraciones v 
                                  else agregarSimbolo v (SymInfo tn (currentScope st) (-1))

procesarDeclaraciones :: [Declaracion]
                      -> Analizador () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = (currentScope s) + 1 })
    mapM_ procesarDeclaracion ds

eliminarDeclaraciones :: [Declaracion]
                      -> Analizador ()
eliminarDeclaraciones ds = do
    mapM_ eliminarDeclaracion ds

--
--  Errores de contexto
--
data ContextError = MultiplesDeclaraciones VarName
                  | VariableNoDeclarada VarName
                  | VariableNoInicializada VarName
                  | TiposNoCoinciden Exp Exp Tipo
                  | TipoIncorrecto Exp Tipo

instance Show ContextError where
    show (MultiplesDeclaraciones v) = "Multiples declaraciones de la variable " ++ v
    show (VariableNoDeclarada v)    = "La variable " ++ v ++ " no ha sido declarada"
    show (VariableNoInicializada v) = "La variable " ++ v ++ " no ha sido inicializada"
    show (TiposNoCoinciden e1 e2 t) = "Los tipos de :\n" ++
        (show e1) ++ "\n" ++
        (show e2) ++ "\n" ++
        "Son incorrectos.\nAmbas expresiones deben ser del tipo: " ++ (show t)
    show (TipoIncorrecto e1 t) = "La expresion :\n" ++
        (show e1) ++ "\n" ++
        "Debe ser del tipo: " ++ (show t) 

instance Error ContextError

--
--  Definicion del analizador de errores de contexto
--

type Analizador a = StateT EvalState (ErrorT ContextError Identity) a

runAnalizador :: Analizador a -> Either ContextError (a, EvalState)
runAnalizador = runIdentity . runErrorT . (flip runStateT initialState)

data EvalState = EvalState {
    currentScope :: Scope,
    tabla        :: SymTable 
} deriving (Show)

initialState :: EvalState
initialState = EvalState {
    currentScope = 0,
    tabla        = tablaVacia
}

analizarI :: Inst -> Analizador () 

analizarI (I_Declare ds is) = do
    procesarDeclaraciones ds
    mapM_ analizarI is
    eliminarDeclaraciones ds
analizarI (I_Assign id exp) = do
    t  <- buscarTipo id
    te <- getType exp
    if t == te 
        then return ()
        else throwError $ TipoIncorrecto exp t
analizarI (I_If cond exito) = do
    chequearTipoDeExpresion cond Tipo_Boolean
    mapM_ analizarI exito
analizarI (I_IfElse cond exito fallo) = do
    chequearTipoDeExpresion cond Tipo_Boolean
    mapM_ analizarI exito
    mapM_ analizarI fallo
analizarI (I_While guardia is)      = do
    chequearTipoDeExpresion guardia Tipo_Boolean
    mapM_ analizarI is 
analizarI (I_For id e1 e2 is) = undefined
analizarI (I_From e1 e2 is)   = undefined
analizarI (I_Write e)         = return ()
analizarI (I_Read id )        = do
    buscarTipo id 
    return ()

chequearTipoDeExpresiones :: Exp -> Exp -> Tipo -> Analizador Tipo
chequearTipoDeExpresiones e1 e2 t = do
    t1 <- getType e1
    t2 <- getType e2
    if t1 == t && t1 == t
        then return t
        else throwError $ TiposNoCoinciden e1 e2 t

chequearTipoDeExpresion :: Exp -> Tipo -> Analizador Tipo
chequearTipoDeExpresion e t = do
    t1 <- getType e
    if t1 == t
        then return t
        else throwError $ TipoIncorrecto e t

getType :: Exp -> Analizador Tipo
getType (E_Const _)          = return Tipo_Integer
getType (E_Var id)           = buscarTipo id 
getType (E_True)             = return Tipo_Boolean
getType (E_False)            = return Tipo_Boolean
getType (E_BinOp op e1 e2) = do
    case op of 
        Op_Sum -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Res -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Mul -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Div -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Con -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean
        Op_Dis -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean
getType (E_Comp op e1 e2)  = do
    case op of 
        Op_Eq  -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Neq -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Gt  -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Geq -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Lt  -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
        Op_Leq -> chequearTipoDeExpresiones e1 e2 Tipo_Integer
    return Tipo_Boolean
getType (E_UnOp op e)    = do
    case op of
        Op_NegArit -> chequearTipoDeExpresion e Tipo_Integer
        Op_NegBool -> chequearTipoDeExpresion e Tipo_Boolean
        Op_Inspecc -> chequearTipoDeExpresion e Tipo_Tape
getType (E_Paren e)     = getType e
getType (E_Corch e)     = undefined

