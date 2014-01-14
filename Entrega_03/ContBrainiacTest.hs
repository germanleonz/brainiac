import LexBrainiac
import SinBrainiac
import ContBrainiac

main :: IO ()
main = do
    s <- getContents
    let (errores, tokens) = lexer s
    if (not . null) errores
        then 
            mapM_ print errores
        else do
            let ast = calc tokens
            analisis ast 

analisis :: Inst -> IO ()
analisis ast = do
    result <- correrAnalizador $ analizar ast 
    case result of
        (Left err) -> putStrLn $ show err
        otherwise  -> print $ ast
