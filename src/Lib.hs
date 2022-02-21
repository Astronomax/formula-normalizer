module Lib (someFunc) where

import IO (parseFormula)
import Parser (runParserFully)

someFunc :: IO ()
someFunc = do
    s <- getLine
    putStrLn $ show $ runParserFully parseFormula s
