import SinBrainiac
import LexBrainiac
import Impresor

main = do
    s <- getContents
    -- Conseguimos los tokens o los errores en caso de que hayan
    let (errores, tokens) = lexer s
    if (not . null) errores
        then 
            --  Si hay errores lexicograficos se imprimen todos
            mapM_ print errores
        else do
            --  Se intenta armar el AST
            --  si hay errores sintacticos se muestra el primero que se encuentre
            let ast = calc tokens
            correrImpresor $ impresor ast -- Si no hay errores se muestra el AST
            return ()
