{-
 -  Entrega 03 - Analisis de contexto
 -}
module ContBrainiac (
    Evaluator,
    runEvaluator,
    evaluate,
    evaluateI,
    parse
)
where

import SinBrainiac

import qualified Data.Map as DM
import Data.Sequence as DS
 
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

--
--  Tabla de simbolos
--

data SymTable = SymTable (DM.Map VarName (Seq SymInfo))
    deriving (Show)

data SymInfo = SymInfo Tipo Scope Valor
    deriving (Show)

type Scope = Int

{-data Valor = Maybe Int-}

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

buscarSymInfo :: VarName
              -> SymTable 
              -> Maybe SymInfo 
buscarSymInfo str (SymTable m) =
    case DM.lookup str m of
        Nothing -> Nothing
        Just s -> case viewl s of
            EmptyL     -> Nothing
            info :< xs -> Just info

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

agregarSimbolo :: VarName
               -> SymInfo
               -> Evaluator ()
agregarSimbolo str info = do
    modify (\s -> s { tabla = insertar str info (tabla s)})

buscarValor :: VarName
            -> Evaluator Int
buscarValor var = do
    state <- get
    case buscarSymInfo var (tabla state) of
        Just (SymInfo _ _ val) -> return val
        Nothing                -> throwError $ VariableNoExiste var

cambiarValor :: VarName
             -> Valor
             -> Evaluator ()
cambiarValor id vn = do
    buscarValor id
    modify $ (\s -> s { tabla = actualizar id vn (currentScope s) (tabla s) })

procesarDeclaracion :: Declaracion
                    -> Evaluator ()
procesarDeclaracion (Decl v t) = do
    st <- get
    case buscarSymInfo v (tabla st) of 
        Nothing              -> agregarSimbolo v (SymInfo t (currentScope st) (-1))
        Just (SymInfo t l val) -> if l == (currentScope st)
                                  then throwError $ MultiplesDeclaraciones v 
                                  else agregarSimbolo v (SymInfo t (currentScope st) (-1))

procesarDeclaraciones :: [Declaracion]
                      -> Evaluator () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = (currentScope s) + 1 })
    mapM_ procesarDeclaracion ds

--
--  Errores de contexto
--
data ContextError = MultiplesDeclaraciones VarName
                  | VariableNoExiste VarName
                  deriving (Show)

instance Error ContextError

--
--  Definicion del evaluador (en un principio analizador de errores de contexto)
--

type Evaluator a = StateT EvalState
                     (ErrorT ContextError Identity) a

runEvaluator :: Evaluator a 
             -> (Either ContextError (a, EvalState))
runEvaluator = runIdentity . runErrorT .
                    (flip runStateT initialState)

data EvalState = EvalState {
    currentScope :: Scope,
    tabla        :: SymTable 
} deriving (Show)

initialState :: EvalState
initialState = EvalState {
    currentScope = 0,
    tabla        = tablaVacia
}

evaluateI :: Inst -> Evaluator () 

evaluateI (I_Declare ds is) = do
    procesarDeclaraciones ds
    mapM_ evaluateI is


evaluateI (I_Assign id exp) = do
    vn <- evaluate exp
    cambiarValor id vn

{-evaluate (I_If cond exito) = do-}
    {-eval_cond <- evaluate cond-}
    {-if (true == eval_cond) then eval exito-}

{-evaluate (I_IfElse cond exito fallo) = do-}
    {-eval_cond <- eval cond-}
    {-if (eval_cond == true) then eval exito-}
                           {-else eval fallo-}

evaluate :: Exp -> Evaluator Int

evaluate (E_Const n) = return n

evaluate (E_Var v) = buscarValor v 

evaluate (E_UnOp op exp) = do
    x <- evaluate exp
    case op of
        Op_NegArit -> return (-x)

evaluate (E_BinOp op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of 
        Op_Sum -> return $ lft + rgt
        Op_Res -> return $ lft - rgt
