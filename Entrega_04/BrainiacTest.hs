import LexBrainiac
import SinBrainiac
import ContBrainiac
import BrainiacMachine

import Control.Monad (liftM)

main :: IO ()
main = do
    s <- getContents
    let (errores, tokens) = lexer s
    if (not . null) errores
        then mapM_ print errores
        else liftM analisis calc tokens 

analisis :: Inst -> IO ()
analisis ast = do
    res <- correrAnalizador $ analizar ast 
    either print (\_ -> ejecutar ast) res

ejecutar :: Inst -> IO ()
ejecutar ast = do
    res <- correrAnalizador $ correr ast 
    either print (\_ -> return ()) res
