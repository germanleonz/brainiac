{
    module LexBrainiax (Main) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
@reserved = declare|execute|while|bool|if|end

tokens :-

    $white+                         ;
    "$$".*                          ;
    "declare"                       { tok (\p s -> TkDeclare p)}
    "execute"                       { tok (\p s -> TkExecute p)}
    "while"                         { tok (\p s -> TkWhile p)}
    "boolean"                       { tok (\p s -> TkBoolean p)} 
    "integer"                       { tok (\p s -> TkInteger p)} 
    "tape"                          { tok (\p s -> TkTape p)} 
    "if"                            { tok (\p s -> TkIf p)}
    "end"                           { tok (\p s -> TkEnd p)}
    $alpha [$alpha $digit]*         { tok (\p s -> TkIdent p s)}
    $digit+                         { tok (\p s -> TkNum p (read s))}
    "true"                          { tok (\p s -> TkTrue)}
    "false"                         { tok (\p s -> TkFalse)}
    ","                             { tok (\p s -> TkComa)}
    "."                             { tok (\p s -> TkPunto)}
    ";"                             { tok (\p s -> TkPuntoYComa)}
    "("                             { tok (\p s -> TkParAbre)}
    ")"                             { tok (\p s -> TkParCierra)}
    "["                             { tok (\p s -> TkCorcheteAbre)}
    "]"                             { tok (\p s -> TkCorcheteCierra)}
    "{"                             { tok (\p s -> TkLlaveAbre)}
    "}"                             { tok (\p s -> TkLlaveCierra)}
    "::"                            { tok (\p s -> TkType)}
    "+"                             { tok (\p s -> TkMas)}
    "-"                             { tok (\p s -> TkMenos)}
    "*"                             { tok (\p s -> TkMult)}
    "/"                             { tok (\p s -> TkDiv)}
    "%"                             { tok (\p s -> TkMod)}
    "/\"                            { tok (\p s -> TkConjuncion)}
    "\/"                            { tok (\p s -> TkDisyuncion)}
    "~"                             { tok (\p s -> TkNegacion)}
    "<"                             { tok (\p s -> TkMenor)}
    "<="                            { tok (\p s -> TkMenorIgual)}
    ">"                             { tok (\p s -> TkMayor)}
    ">="                            { tok (\p s -> TkMayorIgual)}
    "="                             { tok (\p s -> TkIgual)}
    "/="                            { tok (\p s -> TkDesigual)}
    "&"                             { tok (\p s -> TkConcat)}
    "#"                             { tok (\p s -> TkInspeccion)}
    ":="                            { tok (\p s -> TkAsignacion)}
{
-- Each right-hand side has type :: AlexPosn -> String -> Token

--  some action helpers:
tok f p s = f p s

--  El tipo token
data Token =
    TkDeclare 	AlexPosn          |
    TkExecute 	AlexPosn          |
    TkWhile 	AlexPosn          |
    TkInteger 	AlexPosn          |
    TkBoolean 	AlexPosn          |
    TkTape 	AlexPosn              |
    TkIf 	AlexPosn              |
    TkDone 	AlexPosn              |
    TkEnd 	AlexPosn              |
    TkIdent 	AlexPosn String   |
    TkNum 	AlexPosn  Int         |
    TkTrue 	AlexPosn              |
    TkFalse 	AlexPosn          |
    TkComa 	AlexPosn              |
    TkPunto 	AlexPosn          |  
    TkPuntoYComa 	AlexPosn      | 
    TkParAbre 	AlexPosn          |
    TkParCierra 	AlexPosn      |  
    TkCorcheteAbre 	AlexPosn      |
    TkCorcheteCierra 	AlexPosn  | 
    TkLlaveAbre 	AlexPosn      |  
    TkLlaveCierra 	AlexPosn      |
    TkType 	AlexPosn              |
    TkMas 	AlexPosn              |
    TkMenos 	AlexPosn          |  
    TkMult 	AlexPosn              |
    TkDiv 	AlexPosn              |
    TkMod 	AlexPosn              |
    TkConjuncion 	AlexPosn      | 
    TkDisyuncion 	AlexPosn      | 
    TkNegacion 	AlexPosn          |
    TkMenor 	AlexPosn          |  
    TkMenorIgual 	AlexPosn      | 
    TkMayor 	AlexPosn          |  
    TkMayorIgual 	AlexPosn      | 
    TkIgual 	AlexPosn          | 
    TkDesigual 	AlexPosn          |
    TkConcat 	AlexPosn          | 
    TkInspeccion 	AlexPosn      |
    TkAsignacion 	AlexPosn       
    deriving (Eq, Show)

token_posn (TkDeclare p) = p
token_posn (TkExecute p) = p
token_posn (TkWhile p) = p 	
token_posn (TkInteger p) = p 	
token_posn (TkBoolean p) = p 	
token_posn (TkTape p) = p 	
token_posn (TkIf p) = p 
token_posn (TkDone p) = p
token_posn (TkEnd p) = p 
token_posn (TkIdent p _) = p
token_posn (TkNum p _) = p 	
token_posn (TkTrue p) = p 	
token_posn (TkFalse p) = p 
token_posn (TkComa p) = p 
token_posn (TkPunto p) = p 	
token_posn (TkPuntoYComa p) = p
token_posn (TkParAbre p) = p 
token_posn (TkParCierra p) = p
token_posn (TkCorcheteAbre p) = p
token_posn (TkCorcheteCierra p) = p
token_posn (TkLlaveAbre p) = p 
token_posn (TkLlaveCierra p) = p
token_posn (TkType p) = p 
token_posn (TkMas p) = p 	
token_posn (TkMenos p) = p 	
token_posn (TkMult p) = p 	
token_posn (TkDiv p) = p 	
token_posn (TkMod p) = p 	
token_posn (TkConjuncion p) = p
token_posn (TkDisyuncion p) = p
token_posn (TkNegacion p) = p 	
token_posn (TkMenor p) = p 
token_posn (TkMenorIgual p) = p
token_posn (TkMayor p) = p 	
token_posn (TkMayorIgual p) = p
token_posn (TkIgual p) = p 	
token_posn (TkDesigual p) = p
token_posn (TkConcat p) = p 	
token_posn (TkInspeccion p) = p 
token_posn (TkAsignacion p) = p

main = do
    s <- getContents
    print (alexScanTokens s)
}
