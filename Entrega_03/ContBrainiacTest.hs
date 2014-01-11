import ContBrainiac

main :: IO ()
main = do
    s <- getContents
    let ast = parse s
    print $ runEvaluator $ evaluateI ast
