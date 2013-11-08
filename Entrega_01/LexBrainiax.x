{
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
import Data.Data
import Data.Typeable
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                       ;
    "$$".*                        ;
    "$-"[$white .]*"-$"           ;
    declare                       { tok (\p s -> TkDeclare p)}
    execute                       { tok (\p s -> TkExecute p)}
    while                         { tok (\p s -> TkWhile p)}
    from                          { tok (\p s -> TkFrom p)}
    to                            { tok (\p s -> TkTo p)}
    do                            { tok (\p s -> TkDo p)}
    done                          { tok (\p s -> TkDone p)}
    boolean                       { tok (\p s -> TkBoolean p)} 
    integer                       { tok (\p s -> TkInteger p)} 
    tape                          { tok (\p s -> TkTape p)} 
    if                            { tok (\p s -> TkIf p)}
    else                          { tok (\p s -> TkElse p)}
    end                           { tok (\p s -> TkEnd p)}
    read                          { tok (\p s -> TkRead p)}
    write                         { tok (\p s -> TkWrite p)}
    true                          { tok (\p s -> TkTrue p)}
    false                         { tok (\p s -> TkFalse p)}
    $alpha [$alpha $digit]*       { tok (\p s -> TkIdent p s)}
    $digit+                       { tok (\p s -> TkNum p (read s))}
    \,                            { tok (\p s -> TkComa p)}
    \.                            { tok (\p s -> TkPunto p)}
    \;                            { tok (\p s -> TkPuntoYComa p)}
    \(                            { tok (\p s -> TkParAbre p)}
    \)                            { tok (\p s -> TkParCierra p)}
    \[                            { tok (\p s -> TkCorcheteAbre p)}
    \]                            { tok (\p s -> TkCorcheteCierra p)}
    \{                            { tok (\p s -> TkLlaveAbre p)}
    \}                            { tok (\p s -> TkLlaveCierra p)}
    "::"                          { tok (\p s -> TkType p)}
    \+                            { tok (\p s -> TkMas p)}
    \-                            { tok (\p s -> TkMenos p)}
    \*                            { tok (\p s -> TkMult p)}
    \/                            { tok (\p s -> TkDiv p)}
    \%                            { tok (\p s -> TkMod p)}
    "/\"                          { tok (\p s -> TkConjuncion p)}
    "\/"                          { tok (\p s -> TkDisyuncion p)}
    \~                            { tok (\p s -> TkNegacion p)}
    \<                            { tok (\p s -> TkMenor p)}
    "<="                          { tok (\p s -> TkMenorIgual p)}
    \>                            { tok (\p s -> TkMayor p)}
    ">="                          { tok (\p s -> TkMayorIgual p)}
    \=                            { tok (\p s -> TkIgual p)}
    "\="                          { tok (\p s -> TkDesigual p)}
    \&                            { tok (\p s -> TkConcat p)}
    \#                            { tok (\p s -> TkInspeccion p)}
    ":="                          { tok (\p s -> TkAsignacion p)}
{

-- Each right-hand side has type :: AlexPosn -> String -> Token

--  some action helpers:
tok f p s = f p s

--  El tipo token
data Token =
    TkDeclare        AlexPosn |
    TkExecute        AlexPosn |
    TkWhile          AlexPosn |
    TkFrom           AlexPosn |
    TkTo             AlexPosn |
    TkDo             AlexPosn |
    TkDone           AlexPosn |
    TkInteger        AlexPosn |
    TkBoolean        AlexPosn |
    TkTape           AlexPosn |
    TkIf             AlexPosn |
    TkElse           AlexPosn |
    TkEnd            AlexPosn |
    TkRead           AlexPosn |
    TkWrite          AlexPosn |
    TkIdent          AlexPosn String |
    TkNum            AlexPosn Int    |
    TkTrue           AlexPosn |
    TkFalse          AlexPosn |
    TkComa           AlexPosn |
    TkPunto          AlexPosn |
    TkPuntoYComa     AlexPosn |
    TkParAbre        AlexPosn |
    TkParCierra      AlexPosn |
    TkCorcheteAbre   AlexPosn |
    TkCorcheteCierra AlexPosn |
    TkLlaveAbre      AlexPosn |
    TkLlaveCierra    AlexPosn |
    TkType           AlexPosn |
    TkMas            AlexPosn |
    TkMenos          AlexPosn |
    TkMult           AlexPosn |
    TkDiv            AlexPosn |
    TkMod            AlexPosn |
    TkConjuncion     AlexPosn |
    TkDisyuncion     AlexPosn |
    TkNegacion       AlexPosn |
    TkMenor          AlexPosn |
    TkMenorIgual     AlexPosn |
    TkMayor          AlexPosn |
    TkMayorIgual     AlexPosn |
    TkIgual          AlexPosn |
    TkDesigual       AlexPosn |
    TkConcat         AlexPosn |
    TkInspeccion     AlexPosn |
    TkAsignacion     AlexPosn
    deriving (Eq, Typeable, Data)

instance Data AlexPosn
instance Typeable AlexPosn

instance Show [Token] where
    show xs = mapM_ print xs

instance Show Token where
    show (TkIdent p id) = "TkIdent(\"" ++ id ++ "\")"
    show (TkNum p n)    = "TkNum(" ++ (show n) ++ ")"
    show t              = showConstr $ toConstr t
    
scanner str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError inp'@((AlexPn _ line column),_,_,r) -> do 
                    error $ "Caracter inesperado " ++ (show $ head r) ++ " en la fila " ++ (show line) ++ ", columna " ++ (show column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

main = do
    s <- getContents
    print (scanner s)
}
