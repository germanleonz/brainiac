{-|
  /Interpretador del lenguaje Brainiac/

   German Leon Z. <08-10611@ldc.usb.ve>
 -}
module BrainiacMachine (
    -- * Funciones exportadas
    correr,
    evaluar
)
where

import Control.Monad.Error
import Data.Char as C
import System.IO

import Cinta
import ContBrainiac
import SinBrainiac
import TablaSim

correrSecuencia :: [Inst] -> Analizador ()
correrSecuencia = mapM_ correr 

correr :: Inst -> Analizador ()

correr (I_Assign id exp) = do 
    v_exp <- evaluar exp
    t_var <- buscarTipo id
    case t_var of
        Tipo_Tape -> case v_exp of
            ValN n | n <= 0 -> throwError CintaMalFormada
            otherwise       -> cambiarValor id $ ValC (cintaVacia $ unpackN v_exp)
        otherwise -> cambiarValor id v_exp
correr (I_If b exito) = correr $ I_IfElse b exito []
correr (I_IfElse b exito fallo) = do
    v_exp <- evaluar b
    case v_exp of
        ValB True -> correrSecuencia exito
        otherwise -> correrSecuencia fallo
correr i@(I_While g cuerpo) = do
    val_g <- evaluar g
    case val_g of
        ValB True -> correrSecuencia cuerpo >> correr i
        otherwise -> continue
correr i@(I_For id inf sup c) = do
    vi <- liftM unpackN $ evaluar inf
    vs <- liftM unpackN $ evaluar sup
    let go vi | vi >  vs = continue
              | vi <= vs = do
            cambiarValor id $ ValN vi
            correrSecuencia c 
            go $ vi + 1
        in  go vi
correr i@(I_From inf sup c) = do
    vi <- liftM unpackN $ evaluar inf
    vs <- liftM unpackN $ evaluar sup
    let go vi | vi >  vs = continue
              | vi <= vs = do
            correrSecuencia c 
            go $ vi + 1
        in  go vi
correr (I_Declare ds is) = do
    procesarDeclaraciones ds
    correrSecuencia is 
    eliminarDeclaraciones ds 
correr (I_Write e) = evaluar e >>= liftIO . print
correr (I_Read id) = do
    t_var <- buscarTipo id
    case t_var of
        Tipo_Integer -> leerEntero   >>= cambiarValor id
        Tipo_Boolean -> leerBooleano >>= cambiarValor id 
        otherwise    -> throwError $ ErrorLecturaCinta id
correr (I_Ejec cadena cinta) = do
    case cinta of 
        (E_Var id) -> do 
            c  <- buscarValor id    
            nc <- ejecutarCadena cadena (unpackC c)
            cambiarValor id (ValC nc)
        otherwise -> do
            n <- evaluar cinta 
            ejecutarCadena cadena (cintaVacia $ unpackN n)
            continue
correr (I_Concat e id) = do
    case id of
        (E_Var id) -> do
            tam <- liftM unpackN $ evaluar e
            c   <- buscarValor id
            case c of
                Null      -> throwError $ VariableNoInicializada id
                otherwise -> continue 
            let nc = extenderCinta tam (unpackC c)
            cambiarValor id (ValC nc)

evaluar :: Exp -> Analizador BrainVal

evaluar (E_Const c) = return $ ValN c
evaluar (E_True)    = return $ ValB True
evaluar (E_False)   = return $ ValB False
evaluar (E_Var id)  = do
    v <- buscarValor id 
    case v of
        Null      -> throwError $ VariableNoInicializada id
        otherwise -> return v
evaluar e@(E_BinOp op e1 e2)   = do
    lv <- evaluar e1
    rv <- evaluar e2
    case op of 
        Op_Sum -> numBinOp (+) lv rv
        Op_Res -> numBinOp (-) lv rv
        Op_Mul -> numBinOp (*) lv rv
        Op_Div | (unpackN rv) == 0 -> throwError $ DivisionPorCero e
               | otherwise         -> numBinOp div lv rv
        Op_Dis -> boolBinOp (||) lv rv
        Op_Con -> boolBinOp (&&) lv rv
evaluar (E_Comp op e1 e2) = do
    lv <- evaluar e1
    rv <- evaluar e2
    case op of 
        Op_Eq  -> case lv of
            (ValN _) -> return $ ValB $ (unpackN lv) == (unpackN rv)
            (ValB _) -> boolBinOp (==) lv rv
        Op_Neq ->  case lv of 
            (ValN _) -> return $ ValB $ (unpackN lv) /= (unpackN rv)
            (ValB _) -> boolBinOp (/=) lv rv
        Op_Lt  -> return $ ValB $ (unpackN lv) <  (unpackN rv)
        Op_Leq -> return $ ValB $ (unpackN lv) <= (unpackN rv)
        Op_Gt  -> return $ ValB $ (unpackN lv) >  (unpackN rv)
        Op_Geq -> return $ ValB $ (unpackN lv) >= (unpackN rv)
evaluar (E_UnOp op e) = do
    v <- evaluar e
    case op of
        Op_NegArit -> return $ ValN $ negate           $ unpackN v
        Op_NegBool -> return $ ValB $ not              $ unpackB v
        Op_Inspecc -> return $ ValN $ conseguirPrimero $ unpackC v
evaluar (E_Paren e) = evaluar e
evaluar (E_Corch e) = evaluar e

boolBinOp op v1 v2 = return $ ValB $ unpackB v1 `op` unpackB v2
numBinOp  op v1 v2 = return $ ValN $ unpackN v1 `op` unpackN v2

unpackN ::  BrainVal -> Int
unpackN (ValN v) = v

unpackB ::  BrainVal -> Bool
unpackB (ValB b) = b

unpackC ::  BrainVal -> Cinta
unpackC (ValC c) = c

leerBooleano :: Analizador BrainVal
leerBooleano = do
    tty <- liftIO $ openFile "/dev/tty" ReadMode
    str <- liftIO $ hGetLine tty
    liftIO $ hClose tty
    case str of 
        "true"    -> return $ ValB True
        "false"   -> return $ ValB False
        otherwise -> throwError $ ErrorDeEntrada Tipo_Boolean

leerEntero :: Analizador BrainVal
leerEntero = do
    tty <- liftIO $ openFile "/dev/tty" ReadMode
    str <- liftIO $ hGetLine tty
    liftIO $ hClose tty
    case all isNumber str of 
        True  -> return $ ValN (read str)
        False -> throwError $ ErrorDeEntrada Tipo_Integer

ejecutarCadena :: [B_Inst] -> Cinta -> Analizador Cinta
ejecutarCadena cadena cinta = foldM evalC cinta cadena

evalC :: Cinta -> B_Inst -> Analizador Cinta
evalC c C_Sum = return $ modificarCasilla inc c
evalC c C_Res = return $ modificarCasilla dec c
evalC c C_Izq = return $ moverPrimero     dec c
evalC c C_Der = return $ moverPrimero     inc c
evalC c C_Imp = do
    liftIO $ putChar $ C.chr (conseguirPrimero c)
    return c
evalC c C_Lee = do
    v <- liftM unpackN leerEntero 
    return $ modificarCasilla (\_ -> (id v)) c

inc :: Int -> Int
inc = (+1) 
          
dec :: Int -> Int
dec x = x - 1
