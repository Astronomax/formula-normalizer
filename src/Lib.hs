module Lib (someFunc) where

import Formula
import Parser (parseFormula)
import FormulaToNNF (formulaToNNF)
import FormulaToDNF (formulaToDNF)
import FormulaToCNF (formulaToCNF)
import System.Environment (getArgs)
import Data.Either
import Control.Monad.Except (Except, runExcept, MonadError (throwError))

data MainError = InvalidArguments | ParsingError

instance Show MainError where
    show InvalidArguments = "InvalidArguments"
    show ParsingError     = "ParsingError"
  
mainExcept :: [String] -> Except MainError Formula
mainExcept args = do
    strFormula <- argsExcept args
    f <- parseExcept strFormula
    return f
    where   argsExcept :: [String] -> Except MainError String
            argsExcept [arg] = return arg
            argsExcept args = throwError $ InvalidArguments

            parseExcept :: String -> Except MainError Formula
            parseExcept strFormula = case parseFormula strFormula of
                Right f -> return f
                Left  s -> throwError ParsingError

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case runExcept $ mainExcept args of
        Right f -> do 
            putStrLn $ "NNF: " ++ (show $ formulaToNNF f)
            putStrLn $ "DNF: " ++ (show $ formulaToDNF f)
            putStrLn $ "CNF: " ++ (show $ formulaToCNF f)
        Left err -> print err

