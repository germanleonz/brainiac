{-
 -  Entrega 03 - Analisis de contexto
 -}
module ContBrainiac (
    Analizador,
    runAnalizador,
    evaluateE,
    evaluateI
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

data SymInfo = SymInfo Tipo Scope Valor
    deriving (Show)

type Scope = Int

{-| 
  Creacion de una tabla de simbolos vacia    
-}
tablaVacia :: SymTable 
tablaVacia = SymTable (DM.empty)

{-|
  Agregar una nueva variable a la tabla de simbolos.
-}
insertar :: VarName                     -- ^ Simbolo que se va a agregar
         -> SymInfo                     -- ^ Tipo Valor y scope donde esta el simbolo
         -> SymTable                    -- ^ La tabla de simbolos donde se va a agregar el simbolo
         -> SymTable                    -- ^ La tabla de simbolos con el elemento incluido
insertar vn info (SymTable m)  =  SymTable $ DM.alter f vn m
    where f Nothing  = Just (DS.singleton info)
          f (Just x) = Just (info <| x)

{-|
    Eliminar una variable del scope especificado
-}
eliminarVariable :: VarName 
                 -> Scope
                 -> SymTable
                 -> SymTable
eliminarVariable v sc m = undefined 

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
    Actualizar el valor de una variable 
 -}
actualizar :: VarName 
           -> Valor
           -> Scope
           -> SymTable
           -> SymTable 
actualizar id vn sc (SymTable m) = SymTable $ DM.alter f id m
    where f Nothing =
              Just $ DS.singleton $ SymInfo Tipo_Integer sc vn
          f (Just is) =
              case viewl is of 
                  (SymInfo t l vv) :< iss -> Just $ (SymInfo t l vn) <| iss

--
--  Funciones para manipular la tabla de simbolos dentro del Analizador
--
agregarSimbolo :: VarName
               -> SymInfo
               -> Analizador ()
agregarSimbolo str info = do
    modify (\s -> s { tabla = insertar str info (tabla s)})

buscarValor :: VarName
            -> Analizador Int
buscarValor var = do
    state <- get
    case buscarSymInfo var (tabla state) of
        Just (SymInfo _ _ val) -> return val
        Nothing                -> throwError $ VariableNoDeclarada var

cambiarValor :: VarName
             -> Valor
             -> Analizador ()
cambiarValor id vn = do
    buscarValor id
    modify $ (\s -> s { tabla = actualizar id vn (currentScope s) (tabla s) })

eliminarVariableM :: VarName 
                  -> Analizador ()
eliminarVariableM v = undefined

procesarDeclaracion :: Declaracion
                    -> Analizador ()
procesarDeclaracion (Decl v t) = do
    st <- get
    case buscarSymInfo v (tabla st) of 
        Nothing              -> agregarSimbolo v (SymInfo t (currentScope st) (-1))
        Just (SymInfo t l val) -> if l == (currentScope st)
                                  then throwError $ MultiplesDeclaraciones v 
                                  else agregarSimbolo v (SymInfo t (currentScope st) (-1))

procesarDeclaraciones :: [Declaracion]
                      -> Analizador () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = (currentScope s) + 1 })
    mapM_ procesarDeclaracion ds

--
--  Errores de contexto
--
data ContextError = MultiplesDeclaraciones VarName
                  | VariableNoDeclarada VarName
                  | VariableNoInicializada VarName

instance Show ContextError where
    show (MultiplesDeclaraciones v) = "Multiples declaraciones de la variable " ++ v
    show (VariableNoDeclarada v)    = "La variable " ++ v ++ " no ha sido declarada"
    show (VariableNoInicializada v) = "La variable " ++ v ++ " no ha sido inicializada"

instance Error ContextError

--
--  Definicion del evaluador (en un principio analizador de errores de contexto)
--

type Analizador a = ErrorT ContextError (StateT EvalState IO) a

{-runAnalizador :: Analizador a -}
             {--> (Either ContextError (a, EvalState))-}
runAnalizador eval = runErrorT (runStateT eval initialState)

data EvalState = EvalState {
    currentScope :: Scope,
    tabla        :: SymTable 
} deriving (Show)

initialState :: EvalState
initialState = EvalState {
    currentScope = 0,
    tabla        = tablaVacia
}

evaluateI :: Inst -> Analizador () 

evaluateI (I_Declare ds is) = do
    procesarDeclaraciones ds
    mapM_ evaluateI is
    {-removerDeclaraciones-}
evaluateI (I_Assign id exp) = do
    vn <- evaluateE exp
    cambiarValor id vn
{-evaluateI (I_If cond exito) = do-}
    {-eval_cond <- evaluate cond-}
    {-if (true == eval_cond) then eval exito-}
{-evaluateI (I_IfElse cond exito fallo) = do-}
    {-eval_cond <- eval cond-}
    {-if (eval_cond == true) then eval exito-}
                           {-else eval fallo-}
evaluateI (I_While b is)      = undefined
evaluateI (I_For id e1 e2 is) = undefined
evaluateI (I_From e1 e2 is)   = undefined
evaluateI (I_Write e)         = undefined
evaluateI (I_Read id )        = undefined

evaluateE :: Exp -> Analizador Int

evaluateE (E_Const n) = return n
evaluateE (E_Var v)   = buscarValor v
evaluateE (E_UnOp op exp) = do
    x <- evaluateE exp
    case op of
        Op_NegArit -> return $ -x
        {-Op_NegBool -> return $ not x-}
        {-Op_Inspecc -> return ??? -}
evaluateE (E_BinOp op left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    case op of 
        Op_Sum -> return $ lft + rgt
        Op_Res -> return $ lft - rgt
        Op_Mul -> return $ lft * rgt
        {-Op_Div -> return $ lft / rgt-}

{-evaluateB :: BoolExp -> Evaluator Bool-}

{-evaluateB (B_Bin op left right) = undefined-}
    {-lft <- evaluate left-}
    {-rgt <- evaluate right-}
    {-case op of -}
        {-Op_Gt  -> return $ lft > rgt-}
        {-Op_Geq -> return $ lft >= rgt-}
        {-Op_Lt  -> return $ lft > rgt-}
        {-Op_Leq -> return $ lft >= rgt-}
        {-Op_Eq  -> return $ lft == rgt-}
        {-Op_Neq -> return $ lft /= rgt-}
{-evaluateB (B_True)    = return True-}
{-evaluateB (B_False)   = return False-}

{-evaluateC :: Cinta -> Evaluator Cinta-}
