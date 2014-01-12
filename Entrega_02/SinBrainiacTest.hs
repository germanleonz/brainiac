import SinBrainiac
import LexBrainiac

main = do
    s <- getContents
    let (errores, tokens) = scanner s
    if null errores
        then correrImpresor $ impresor $ calc tokens
        else mapM_ print errores
