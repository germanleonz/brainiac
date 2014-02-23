import LexBrainiac
import SinBrainiac
import ContBrainiac

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
    either (putStrLn . show) (\_ -> print ast) res
