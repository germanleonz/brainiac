import LexBrainiac

main = do
    s <- getContents
    --  Utilizamos el lexer generado por Alex para tokenizar el string s
    --  que representa un programa en Brainiac
    let (errores, tokens) = lexer s
    if null errores
        -- Si no se encontraron errores se imprimen los tokens generados
        then mapM_ print tokens
        -- Si se encontraron errores se imprimen todos
        else mapM_ print errores
