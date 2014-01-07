import LexBrainiac

main = do
    s <- getContents
    let (errores, tokens) = scanner s
    if null errores
        then mapM_ print tokens
        else mapM_ print errores
