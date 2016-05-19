import Data.List
import System.IO

path = '/' : []
    
main :: IO ()
main = do
    line <- getLine
    if null line
        then return ()
	else do
            putStrLn $ identify line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

identify :: String -> String
identify x 
    |head (words (x)) =="cmd1" = "1"
    |head (words (x)) =="cmd2" = "2"
    |otherwise = "Sintaxis Error"