module Lib (someFunc) where

import Formula
import Parser (parseFormula)
import FormulaToNNF (formulaToNNF)
import FormulaToDNF (formulaToDNF)
import Data.Either

someFunc :: IO ()
someFunc = do
    s <- getLine
    let (Right f) = parseFormula s
    putStrLn $ show $ formulaToDNF f