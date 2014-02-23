import LexBrainiac

main :: IO ()
main = do
    s <- getContents
    --  Utilizamos el lexer generado por Alex para tokenizar el string s
    --  que representa un programa en Brainiac
    let (errores, tokens) = lexer s
    if (not . null) errores
        -- Si se encontraron errores se imprimen todos
        then mapM_ print errores
        -- Si no se encontraron errores se imprimen los tokens generados
        else mapM_ (putStr . (\s -> show s ++ " ")) tokens
