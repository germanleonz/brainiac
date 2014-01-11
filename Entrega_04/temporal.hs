evaluate (E_UnOp op exp) = do
    x <- evaluate exp
    case op of
        Op_NegArit -> return (-x)

evaluate (E_BinOp op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of 
        Op_Sum -> return $ lft + rgt
        Op_Res -> return $ lft - rgt

evaluate (E_Const n) = return n

evaluate (E_Var v) = lookUp v 
