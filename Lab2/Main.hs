import System.IO
import Tokens
import Grammar

main :: IO()
main = do
    handle <- openFile "program.clc" ReadMode
    c <- hGetContents handle
    print $ (parseCalc . alexScanTokens) c
    hClose handle

tokenPosn :: Token -> String
tokenPosn (TokenLet (AlexPn _ line col)) = show line ++ "," ++ show col
tokenPosn _ = "" 