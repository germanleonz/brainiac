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
            case runAnalizador $ analizarI ast of
                Left err  -> putStrLn $ show err
                Right _ -> do
                    correrImpresor $ impresor ast
                    return ()
