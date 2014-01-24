{-|
  /Interpretador del lenguaje Brainiac/

   German Leon Z. <08-10611@ldc.usb.ve>
 -}
module BrainiacMachine (
    -- * Tipos exportados
    TablaSim,
    -- * Funciones exportadas
    {-runBrainiacProgram,-}
    correrAnalizador,
    analizar,
    correr,
    evaluar,
)
where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Data.Char as C
import Data.Sequence as DS
import qualified Data.Map as DM

import SinBrainiac

------------------------------------------------------------------------
--  Tabla de simbolos basada en una tabla de hash con Strings como    --
--  claves y un Data.Sequence de InfoSim como valores                 --
------------------------------------------------------------------------

{-instance TablaSim t where-}

data TablaSim = TablaSim (DM.Map VarName (Seq InfoSim))
    deriving (Show)

data InfoSim = InfoSim {
    tipo    :: Tipo,
    scope   :: Scope,
    valor   :: BrainVal,
    ocupado :: Bool
} deriving (Show)

type Scope = Int

data BrainVal = ValorNum Int 
              | ValorBool Bool
              | ValorCinta Cinta 
              | Null
              deriving (Eq)

instance Show BrainVal where 
    show (ValorNum v)   = show v
    show (ValorBool v)  = show v
    show (ValorCinta v) = show v

{-| 
    Crear una tabla de simbolos vacia    
-}
tablaVacia :: TablaSim 
tablaVacia = TablaSim (DM.empty)

{-|
    Agregar la variable v a la tabla de simbolos
-}
insertar :: VarName    -- ^ Simbolo que se va a agregar
         -> InfoSim    -- ^ Tipo Valor y scope donde esta el simbolo
         -> TablaSim   -- ^ La tabla de simbolos donde se va a agregar el simbolo
         -> TablaSim   -- ^ La tabla de simbolos con el elemento incluido
insertar vn info (TablaSim m) = TablaSim $ DM.alter f vn m
    where f Nothing  = Just (DS.singleton info)
          f (Just x) = Just (info <| x)

{-|
    Eliminar la variable v del scope mas interno
-}
eliminarVariable :: VarName -> TablaSim -> TablaSim
eliminarVariable v (TablaSim m) = TablaSim $ DM.alter f v m
    where f Nothing  = Just DS.empty
          f (Just s) = Just $ DS.drop 1 s

{-|
    Buscar la informacion relacionada con una variable en la tabla de simbolos
-}
buscarInfoSim :: VarName -> TablaSim -> Maybe InfoSim 
buscarInfoSim id (TablaSim m) =
    case DM.lookup id m of
        Nothing -> Nothing
        Just s -> case viewl s of
            EmptyL     -> Nothing
            info :< xs -> Just info

{-|
    Actualizar el valor de una variable, solo si la variable esta 
    en la tabla de simbolos
 -}
actualizar :: VarName -> BrainVal -> TablaSim -> TablaSim 
actualizar id vn (TablaSim m) = TablaSim $ DM.alter f id m
    where f (Just is) =
            case viewl is of 
                (InfoSim t l vv b) :< iss -> Just $ (InfoSim t l vn b) <| iss

--------------------------------------------------------------------------
--  Funciones para manipular la tabla de simbolos dentro del Analizador --
--------------------------------------------------------------------------

{-|
    Agregamos informacion de la variable id a la tabla de simbolos
-}
agregarSimbolo :: VarName -> InfoSim -> Analizador ()
agregarSimbolo id info = modify (\s -> s { tabla = insertar id info (tabla s)})

{-cambiar las proximas tres funciones por una sola-}

{-|
    Buscamos variable id en el scope mas interno esta bloqueada
    En caso de que no este en la tabla de simbolos se lanza un error
-}
buscarOcupada :: VarName -> Analizador Bool
buscarOcupada id = do
    tabla <- gets tabla
    maybe fallo exito (buscarInfoSim id tabla) 
    where exito = return . ocupado
          fallo = throwError $ VariableNoDeclarada id

{-|
    Buscamos el tipo de datos de la variable id en el scope mas interno.
    En caso de que no este en la tabla de simbolos se lanza un error
-}
buscarTipo :: VarName -> Analizador Tipo
buscarTipo id = do
    tabla <- gets tabla
    maybe fallo exito (buscarInfoSim id tabla) 
    where exito = return . tipo 
          fallo = throwError $ VariableNoDeclarada id

{-|
    Buscamos el valor de la variable id en el scope mas interno.
    En caso de que no este en la tabla de simbolos se lanza un error
 -}
buscarValor :: VarName -> Analizador BrainVal
buscarValor id = do
    tabla <- gets tabla
    maybe fallo exito (buscarInfoSim id tabla) 
    where exito = return . valor
          fallo = throwError $ VariableNoDeclarada id

{-|
    Cambia el valor de la variable id por el valor vn en
    la tabla de simbolos
 -}
cambiarValor :: VarName -> BrainVal -> Analizador ()
cambiarValor id vn = do
    {-liftIO $ putStrLn $ "ASIGNANDOLE " ++ (show vn) ++ " a la variable " ++ id-}
    estaOcupada <- buscarOcupada id
    if estaOcupada
        then throwError $ VariableDeIteracion id      
        else modify $ (\s -> s { tabla = actualizar id vn (tabla s) })

{-cambiar las dos funciones anteriores por una sola-}

{-|
    Marcamos a la variable id como bloqueada en la tabla de simbolos
-}
marcarVariableOcupada :: VarName -> Analizador ()
marcarVariableOcupada id = do
    tabla <- gets tabla
    case buscarInfoSim id tabla of
        Just (InfoSim t l val b) -> do
            eliminarDeclaracion (Decl id t) 
            agregarSimbolo id (InfoSim t l val True)
        otherwise -> continue

{-|
    Marcamos a la variable id como libre en la tabla de simbolos
-}
marcarVariableLibre :: VarName -> Analizador ()
marcarVariableLibre id = do
    tabs <- gets tabla
    case buscarInfoSim id tabs of
        Just (InfoSim t l val b) -> do
            eliminarDeclaracion (Decl id t) 
            agregarSimbolo id (InfoSim t l val False)
        otherwise -> continue

{-|
    Eliminamos la declaracion d de la tabla de simbolos
-}
eliminarDeclaracion :: Declaracion -> Analizador ()
eliminarDeclaracion (Decl v _) = modify (\s -> s { tabla = eliminarVariable v (tabla s)})

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
        Just (InfoSim _ _ _ _) | otherwise -> agregarSimbolo id info

{-|
    Agregamos a la tabla de simbolos las variables definidas en ds
    y aumentamos el scope actual
-}
procesarDeclaraciones :: [Declaracion] -> Analizador () 
procesarDeclaraciones ds = do
    modify (\s -> s { currentScope = (currentScope s) + 1 })
    mapM_ procesarDeclaracion ds
            
{-|
    Eliminamos de la tabla de simbolos las variables declaradas en ds
    y disminuimos en uno el scope actual
-}
eliminarDeclaraciones :: [Declaracion] -> Analizador ()
eliminarDeclaraciones ds = do
    mapM_ eliminarDeclaracion ds
    modify (\s -> s { currentScope = (currentScope s) - 1 })

------------------------------------------------------------------------
--  Definicion del Monad analizador de errores de contexto            --
------------------------------------------------------------------------

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
                   | DivisionPorCero
                   | CintaMalFormada
                   | ErrorDeEntrada Tipo

instance Show BrainiacError where
    show (MultiplesDeclaraciones id) = "Error estatico: " ++
        "Redeclaracion de la variable '" ++ id ++ "'"
    show (VariableNoDeclarada id)    = "Error estatico: " ++
        "La variable '" ++ id ++ "' no ha sido declarada"
    show (VariableNoInicializada id) = "Error estatico: " ++
        "La variable '" ++ id ++ "' no ha sido inicializada"
    show (TiposNoCoinciden e1 e2 t) = "Los tipos de :\n" ++
        (show e1) ++ 
        (show e2) ++
        "Son incorrectos. Ambas expresiones deben ser de " ++ (show t)
    show (TipoIncorrecto e1 t) = "La expresion: \n" ++
        (show e1) ++
        "Debe ser del tipo: " ++ (show t) 
    show (VariableDeIteracion id) = "Error estatico: " ++
        "La variable: '" ++ id ++ "' no puede ser modificada en el ciclo"
    show (ErrorLecturaCinta id) = "Error estatico: " ++ 
        "En la variable v '" ++ id ++ " no pueden leerse cintas " ++
        "de la entrada estandar"
    show (DivisionPorCero)  = "Error dinamico: division por cero"
    show (CintaMalFormada)  = "Error dinamico: una cinta no puede" ++
        " tener tamano negativo"
    show (ErrorDeEntrada t)  = "Error dinamico de lectura se esperaba un " ++
        "valor de tipo " ++ (show t)

instance Error BrainiacError

analizar :: Inst -> Analizador () 

analizar (I_Declare ds is) = do
    procesarDeclaraciones ds
    analizarInstrucciones is
    eliminarDeclaraciones ds
analizar i@(I_Assign id exp) = do
    --  Revisar si id es una variable ligada a una iteracion
    estaOcupada <- buscarOcupada id
    if estaOcupada 
        then throwError $ VariableDeIteracion id
        else continue
    t_var <- buscarTipo id
    tabs  <- gets tabla
    case t_var of 
        --  Si id es tipo cinta entonces exp debe ser:
        --  [ E ] donde E es de tipo entero 
        Tipo_Tape -> do
            case exp of
                (E_Corch ec) -> chequearTipoDeExpresion     ec  Tipo_Integer
                otherwise    -> throwError $ TipoIncorrecto exp Tipo_Integer
            continue
        otherwise -> do
            --  En caso contrario se verifica que ambos lados de la asignacion
            --  sean del mismo tipo 
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
analizar i@(I_Write e)  = continue
analizar i@(I_Read id ) = do
    t <- buscarTipo id 
    case t of
        Tipo_Tape -> throwError $ ErrorLecturaCinta id
        otherwise -> continue
analizar i@(I_Ejec cadena e) = do
    case e of
        (E_Var _)    -> chequearTipoDeExpresion e Tipo_Tape
        (E_Corch ec) -> chequearTipoDeExpresion e Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e Tipo_Tape
    continue
analizar i@(I_Concat e1 e2) = do
    case e1 of
        (E_Var _)    -> chequearTipoDeExpresion e1 Tipo_Tape
        (E_Corch ec) -> chequearTipoDeExpresion e1 Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e1 Tipo_Tape
    case e2 of
        (E_Var _)    -> chequearTipoDeExpresion e2 Tipo_Tape
        (E_Corch ec) -> chequearTipoDeExpresion ec Tipo_Integer
        otherwise    -> throwError $ TipoIncorrecto e1 Tipo_Tape
    continue

analizarInstrucciones :: [Inst] -> Analizador ()
analizarInstrucciones = mapM_ analizar

--  Dada una expresion conseguir el tipo

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
    chequearTipoDeExpresiones e1 e2 Tipo_Integer
    {-case op of-}
        {-Op_Eq  -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean-}
        {-Op_Neq -> chequearTipoDeExpresiones e1 e2 Tipo_Boolean -}
    return Tipo_Boolean
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

------------------------------------------------------------------------
--  Corrida de un programa en Brainiac                                --
------------------------------------------------------------------------

correr :: Inst -> Analizador ()

correr (I_Assign id exp) = do 
    v_exp <- evaluar exp
    t_var <- buscarTipo id
    case t_var of
        --  Construccion de una cinta
        Tipo_Tape -> do
            case v_exp of
                ValorNum n | n <= 0 -> throwError $ CintaMalFormada
                otherwise -> cambiarValor id (ValorCinta (cintaVacia $ unpackNum v_exp))
        --  Asignacion de la evaluacion de una expresion en un entero o un booleano
        otherwise -> cambiarValor id v_exp
correr (I_If b exito) = do 
    v_exp <- evaluar b 
    case v_exp of
        (ValorBool True) -> correrSecuencia exito
        otherwise        -> continue
correr (I_IfElse b exito fallo) = do
    v_exp <- evaluar b
    case v_exp of
        (ValorBool True) -> correrSecuencia exito
        otherwise        -> correrSecuencia fallo
correr i@(I_While g cuerpo) = do
    val_g <- evaluar g
    case val_g of
        ValorBool True -> correrSecuencia cuerpo >> correr i
        otherwise      -> continue
correr i@(I_For id inf sup c) = do
    vinf <- evaluar inf
    vsup <- evaluar sup
    let val_inf = unpackNum vinf
    let val_sup = unpackNum vsup
    let go val_inf | val_inf >  val_sup = continue
        go val_inf | val_inf <= val_sup = do
            cambiarValor id (ValorNum val_inf)
            correrSecuencia c 
            go (val_inf + 1)
        in go val_inf
correr i@(I_From inf sup c) = do
    vinf <- evaluar inf
    vsup <- evaluar sup
    let val_inf = unpackNum vinf
    let val_sup = unpackNum vsup
    let go val_inf | val_inf >  val_sup = continue
        go val_inf | val_inf <= val_sup = do
            correrSecuencia c 
            go $ val_inf + 1
        in go val_inf
correr (I_Declare ds is) = do
    procesarDeclaraciones ds
    correrSecuencia is 
    eliminarDeclaraciones ds 
correr (I_Write e) = evaluar e >>= liftIO . putStrLn . show
correr (I_Read id) = do
    -- Conseguir un valor de la consola 
    -- si es true o false verificar que id sea tipo boolean y hacer cambioValor
    -- si es un numero verificar que id sea tipo integer y hacer cambiarvalor
    -- en cualquier otro caso error de entrada dinamico
    t_var <- buscarTipo id
    case t_var of
        Tipo_Integer -> do
            val <- leerEntero 
            cambiarValor id val
        Tipo_Boolean -> do
            val <- leerBooleano 
            cambiarValor id val
        otherwise    -> throwError $ ErrorLecturaCinta id
correr (I_Ejec cadena cinta) = do
    case cinta of 
        (E_Var id) -> do 
            c  <- buscarValor id    
            nc <- ejecutarCadena cadena (unpackCinta c)
            cambiarValor id (ValorCinta nc)
        otherwise -> do
            n <- evaluar cinta 
            ejecutarCadena cadena (cintaVacia $ unpackNum n)
            continue
correr (I_Concat c1 c2)      = do
    continue

leerBooleano :: Analizador BrainVal
leerBooleano = do
    liftIO $ putStr "hola"
    str <- liftIO getLine
    {-let str = "true"-}
    case str of 
        "true"    -> return $ ValorBool True
        "false"   -> return $ ValorBool False
        otherwise -> throwError $ ErrorDeEntrada Tipo_Boolean

leerEntero :: Analizador BrainVal
leerEntero = do
    liftIO $ putStr "hola"
    str <- liftIO getLine
    {-let str = "3"-}
    case all isNumber str of 
        False     -> throwError $ ErrorDeEntrada Tipo_Integer
        otherwise -> return $ ValorNum (read str)

continue :: Analizador ()
continue = return ()

correrSecuencia :: [Inst] -> Analizador ()
correrSecuencia = mapM_ correr 

--
--  Evaluacion de expresiones
--

evaluar :: Exp -> Analizador BrainVal

evaluar (E_Const c) = return $ ValorNum c
evaluar (E_True)    = return $ ValorBool True
evaluar (E_False)   = return $ ValorBool False
evaluar (E_Var id)  = do
    v <- buscarValor id 
    case v of
        Null      -> throwError $ VariableNoInicializada id
        otherwise -> return v
evaluar (E_BinOp op e1 e2)   = do
    lv <- evaluar e1
    rv <- evaluar e2
    case op of 
        Op_Sum -> numBinOp (+) lv rv
        Op_Res -> numBinOp (-) lv rv
        Op_Mul -> numBinOp (+) lv rv
        Op_Div | (unpackNum rv) == 0 -> throwError DivisionPorCero
        Op_Div | otherwise -> numBinOp div lv rv
        Op_Dis -> boolBinOp (||) lv rv
        Op_Con -> boolBinOp (&&) lv rv
evaluar (E_Comp op e1 e2) = do
    lv <- evaluar e1
    rv <- evaluar e2
    case op of 
        Op_Eq  -> return $ ValorBool $ (unpackNum lv) == (unpackNum rv)
        Op_Neq -> return $ ValorBool $ (unpackNum lv) /= (unpackNum rv)
        Op_Lt  -> return $ ValorBool $ (unpackNum lv) <  (unpackNum rv)
        Op_Leq -> return $ ValorBool $ (unpackNum lv) <= (unpackNum rv)
        Op_Gt  -> return $ ValorBool $ (unpackNum lv) >  (unpackNum rv)
        Op_Geq -> return $ ValorBool $ (unpackNum lv) >= (unpackNum rv)
evaluar (E_UnOp op e) = do
    v <- evaluar e
    case op of
        Op_NegArit -> return $ ValorNum  $ negate $ unpackNum v
        Op_NegBool -> return $ ValorBool $ not $ unpackBool v
        Op_Inspecc -> return $ ValorNum  $ conseguirPrimero $ unpackCinta v
evaluar (E_Paren e) = evaluar e
evaluar (E_Corch e) = evaluar e

boolBinOp op arg1 arg2 = return $ ValorBool $ unpackBool arg1 `op` unpackBool arg2
numBinOp  op arg1 arg2 = return $ ValorNum  $ unpackNum  arg1 `op` unpackNum arg2

--
--  Definicion de la cinta y las operaciones sobre ella
--

data Cinta = Cinta {
    primero :: Int,
    valores :: DM.Map Int Int,
    tamano  :: Int
} deriving (Show, Eq)

cintaVacia :: Int -> Cinta
cintaVacia n = Cinta 0 DM.empty n

conseguirPrimero :: Cinta -> Int 
conseguirPrimero c = DM.findWithDefault 0 (primero c) (valores c)

moverPrimero :: (Int -> Int) -> Cinta -> Cinta
moverPrimero fn c = c { primero = np }
    where 
        np = (fn $ primero c) `mod` t
        t  = tamano c

modificarCasilla :: (Int -> Int) -> Cinta -> Cinta
modificarCasilla fn c = c { valores = nuevosValores }
    where 
        primeroAnterior = conseguirPrimero c
        nuevosValores   = DM.insert (primero c) (fn primeroAnterior) (valores c)

ejecutarCadena :: [B_Inst] -> Cinta -> Analizador Cinta
ejecutarCadena cadena cinta = foldM evalC cinta cadena

evalC :: Cinta -> B_Inst -> Analizador Cinta
evalC c (C_Sum) = return $ modificarCasilla inc c
evalC c (C_Res) = return $ modificarCasilla dec c
evalC c (C_Izq) = return $ moverPrimero dec c
evalC c (C_Der) = return $ moverPrimero inc c
evalC c (C_Imp) = do
    liftIO $ putChar $ C.chr (conseguirPrimero c)
    return c
evalC c (C_Lee) = do
    {-liftIO $ readInt -}
    return c

inc :: Int -> Int
inc = (+1) 
          
dec :: Int -> Int
dec x = x - 1

unpackNum   (ValorNum v)   = v
unpackBool  (ValorBool b)  = b
unpackCinta (ValorCinta c) = c
