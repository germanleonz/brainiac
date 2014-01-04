import SinBrainiac

main = do
    s <- getContents
    let (errores, prog) = happyParse s
    if null errores
        then mapM_ print prog
        else mapM_ print errores
