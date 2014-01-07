import SinBrainiac

main :: IO ()
main = interact (show . runCalc)
