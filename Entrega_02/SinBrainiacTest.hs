import SinBrainiac

main = do
    s <- getContents
    let (errores, prog) = parse s
    if null errores
        then mapM_ print prog
        else mapM_ print errores
