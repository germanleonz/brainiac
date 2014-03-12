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

import           Control.Monad.Error
import           Data.Char as C
import qualified Data.Foldable as DF
import           Data.Sequence
import           System.IO

import Cinta
import ContBrainiac
import Language
import TablaSim

correrSecuencia :: Seq Inst -> Analizador ()
correrSecuencia = DF.mapM_ correr 

correr :: Inst -> Analizador ()

correr (IAssign id exp) = do 
    v_exp <- evaluar exp
    t_var <- buscarTipo id
    case t_var of
        TipoTape -> case v_exp of
            ValN n | n <= 0 -> throwError CintaMalFormada
            otherwise       -> cambiarValor id $ ValC (cintaVacia $ unpackN v_exp)
        otherwise -> cambiarValor id v_exp
correr (IIf b exito) = correr $ IIfElse b exito empty
correr (IIfElse b exito fallo) = do
    v_exp <- evaluar b
    case v_exp of
        ValB True -> correrSecuencia exito
        otherwise -> correrSecuencia fallo
correr i@(IWhile g cuerpo) = do
    val_g <- evaluar g
    case val_g of
        ValB True -> correrSecuencia cuerpo >> correr i
        otherwise -> continue
correr i@(IFor id inf sup c) = do
    vi <- liftM unpackN $ evaluar inf
    vs <- liftM unpackN $ evaluar sup
    let go vi | vi >  vs = continue
              | vi <= vs = do
            cambiarValor id $ ValN vi
            correrSecuencia c 
            go $ vi + 1
        in  go vi
correr i@(IFrom inf sup c) = do
    vi <- liftM unpackN $ evaluar inf
    vs <- liftM unpackN $ evaluar sup
    let go vi | vi >  vs = continue
              | vi <= vs = do
            correrSecuencia c 
            go $ vi + 1
        in  go vi
correr (IDeclare ds is) = do
    procesarDeclaraciones ds
    correrSecuencia is 
    eliminarDeclaraciones ds 
correr (IWrite e) = evaluar e >>= liftIO . print
correr (IRead id) = do
    t_var <- buscarTipo id
    case t_var of
        TipoInteger -> leerEntero   >>= cambiarValor id
        TipoBoolean -> leerBooleano >>= cambiarValor id 
        otherwise    -> throwError $ ErrorLecturaCinta id
correr (IEjec cadena cinta) = do
    case cinta of 
        (EVar id) -> do 
            c  <- buscarValor id    
            nc <- ejecutarCadena cadena (unpackC c)
            cambiarValor id (ValC nc)
        otherwise -> do
            n <- evaluar cinta 
            ejecutarCadena cadena (cintaVacia $ unpackN n)
            continue
correr (IConcat e id) = do
    case id of
        (EVar id) -> do
            tam <- liftM unpackN $ evaluar e
            c   <- buscarValor id
            case c of
                Null      -> throwError $ VariableNoInicializada id
                otherwise -> continue 
            let nc = extenderCinta tam (unpackC c)
            cambiarValor id (ValC nc)

evaluar :: Exp -> Analizador BrainVal

evaluar (EConst c) = return $ ValN c
evaluar (ETrue)    = return $ ValB True
evaluar (EFalse)   = return $ ValB False
evaluar (EVar id)  = do
    v <- buscarValor id 
    case v of
        Null      -> throwError $ VariableNoInicializada id
        otherwise -> return v
evaluar e@(EBinOp op e1 e2)   = do
    lv <- evaluar e1
    rv <- evaluar e2
    case op of 
        OpSum -> numBinOp (+) lv rv
        OpRes -> numBinOp (-) lv rv
        OpMul -> numBinOp (*) lv rv
        OpDiv | (unpackN rv) == 0 -> throwError $ DivisionPorCero e
               | otherwise         -> numBinOp div lv rv
        OpDis -> boolBinOp (||) lv rv
        OpCon -> boolBinOp (&&) lv rv
evaluar (EComp op e1 e2) = do
    lv <- evaluar e1
    rv <- evaluar e2
    case op of 
        OpEq  -> case lv of
            (ValN _) -> return $ ValB $ (unpackN lv) == (unpackN rv)
            (ValB _) -> boolBinOp (==) lv rv
        OpNeq ->  case lv of 
            (ValN _) -> return $ ValB $ (unpackN lv) /= (unpackN rv)
            (ValB _) -> boolBinOp (/=) lv rv
        OpLt  -> return $ ValB $ (unpackN lv) <  (unpackN rv)
        OpLeq -> return $ ValB $ (unpackN lv) <= (unpackN rv)
        OpGt  -> return $ ValB $ (unpackN lv) >  (unpackN rv)
        OpGeq -> return $ ValB $ (unpackN lv) >= (unpackN rv)
evaluar (EUnOp op e) = do
    v <- evaluar e
    case op of
        OpNegArit -> return $ ValN $ negate           $ unpackN v
        OpNegBool -> return $ ValB $ not              $ unpackB v
        OpInspecc -> return $ ValN $ conseguirPrimero $ unpackC v
evaluar (EParen e) = evaluar e
evaluar (ECorch e) = evaluar e

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
        otherwise -> throwError $ ErrorDeEntrada TipoBoolean

leerEntero :: Analizador BrainVal
leerEntero = do
    tty <- liftIO $ openFile "/dev/tty" ReadMode
    str <- liftIO $ hGetLine tty
    liftIO $ hClose tty
    case all isNumber str of 
        True  -> return $ ValN (read str)
        False -> throwError $ ErrorDeEntrada TipoInteger

ejecutarCadena :: Seq BInst -> Cinta -> Analizador Cinta
ejecutarCadena cadena cinta = foldMS evalC cinta cadena
    where foldMS :: (Cinta -> BInst -> Analizador Cinta) -> Cinta -> Seq BInst -> Analizador Cinta
          foldMS f a xs = case viewl xs of 
                            EmptyL  -> return a 
                            x :< xs -> f a x >>= \fax -> foldMS f fax xs

evalC :: Cinta -> BInst -> Analizador Cinta
evalC c CSum = return $ modificarCasilla inc c
evalC c CRes = return $ modificarCasilla dec c
evalC c CIzq = return $ moverPrimero     dec c
evalC c CDer = return $ moverPrimero     inc c
evalC c CImp = do
    liftIO $ putChar $ C.chr (conseguirPrimero c)
    return c
evalC c CLee = do
    v <- liftM unpackN leerEntero 
    return $ modificarCasilla (const v) c

inc :: Int -> Int
inc = (+1) 
          
dec :: Int -> Int
dec x = x - 1
