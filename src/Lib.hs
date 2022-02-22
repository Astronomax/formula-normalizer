module Lib (someFunc) where

import Formula
import Parser (parseFormula)
import FormulaToNNF (formulaToNNF)
import Data.Either

someFunc :: IO ()
someFunc = do
    s <- getLine
    let (Right f) = parseFormula s
    putStrLn $ show $ formulaToNNF f