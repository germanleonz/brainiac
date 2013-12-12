import LexBrainiac

main = do
    s <- getContents
    let (errores, tokens) = lexer s
    if null errores
        then mapM_ print tokens
        else mapM_ print errores
