module Lib (someFunc) where

import Formula
import Parser (parseFormula)

someFunc :: IO ()
someFunc = do
    s <- getLine
    putStrLn $ show $ parseFormula s