import LexBrainiac
import SinBrainiac
import BrainiacMachine

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
    res <- correrAnalizador $ analizar ast 
    case res of
        (Left err)     -> putStrLn $ show err
        (Right (_, s)) -> ejecutar ast

ejecutar :: Inst -> IO ()
ejecutar ast = do
    res <- correrAnalizador $ correr ast 
    case res of
        (Left err)     -> putStrLn $ show err
        (Right (_, s)) -> return ()
