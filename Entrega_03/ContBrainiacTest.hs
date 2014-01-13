import LexBrainiac
import SinBrainiac
import ContBrainiac
import Impresor

main :: IO ()
main = do
    s <- getContents
    let (errores, tokens) = lexer s
    if (not . null) errores
        then 
            mapM_ print errores
        else do
            let ast = calc tokens
            {-runAnalizador $ evaluateI ast-}
            correrImpresor $ impresor ast
            return ()
