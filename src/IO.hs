{-# LANGUAGE FlexibleContexts #-}
module IO (parseFormula) where

import Parser
import Formula
import Data.Foldable (Foldable(foldl'))
import Control.Applicative (empty, Alternative (many, (<|>)))
import Data.Char (isSpace, isAlphaNum)
import Control.Monad.Except(MonadError(throwError, catchError))

char :: Char -> Parser Char
char c = satisfy (== c) 

spaces :: Parser String
spaces = many $ satisfy isSpace

string :: String -> Parser String
string = foldl' appendChar emptyParser
    where appendChar p c = (++) <$> p <*> (pure <$> char c)  
          emptyParser = pure ""

parseT :: Parser Formula
parseT = (char '1') >> spaces >> (pure T)

parseF :: Parser Formula
parseF = (char '0') >> spaces >> (pure F)

parseVar :: Parser Formula
parseVar = Var <$> parseSymb
    where parseSymb = do
            s <- many $ satisfy isAlphaNum
            _ <- spaces
            return s

parseInBrackets :: Parser Formula
parseInBrackets = do
    char '('
    spaces
    x <- parseFormula
    char ')'
    spaces
    return x

parseWithoutBrackets :: Parser Formula
parseWithoutBrackets = parseDImpl <|>
                parseImpl <|>
                parseOr <|>
                parseAnd <|>
                parseAtom

parseFormula :: Parser Formula
parseFormula = spaces >>
                parseInBrackets <|>
                parseWithoutBrackets

parseNot :: Parser Formula
parseNot =  (do
                string "~"
                a <- parseInBrackets
                return $ Not a) <|> 
            (do
                string "~"
                a <- parseWithoutBrackets
                return $ merge a)
                where   merge :: Formula -> Formula
                        merge T = Not T
                        merge F = Not F
                        merge (Not b) = Not (merge b)
                        merge a@(Var b) = Not a
                        merge (And b c) = And (merge b) c
                        merge (Or b c) = Or (merge b) c
                        merge (Impl b c) = Impl (merge b) c
                        merge (DImpl b c) = DImpl (merge b) c
    
parseDImpl :: Parser Formula
parseDImpl = do
    a <- parseAtom
    string "<->"
    b <- parseFormula
    spaces
    return $ merge a b
    where   merge :: Formula -> Formula -> Formula
            merge a T = DImpl a T
            merge a F = DImpl a F
            merge a b@(Not c) = DImpl a b
            merge a b@(Var c) = DImpl a b
            merge a b@(And c d) = DImpl a b
            merge a b@(Or c d) = DImpl a b
            merge a b@(Impl c d) = DImpl a b
            merge a (DImpl c d) = DImpl (DImpl a c) d

parseImpl :: Parser Formula
parseImpl = do
    a <- parseAtom
    string "->"
    b <- parseFormula
    spaces
    return $ merge a b
    where   merge :: Formula -> Formula -> Formula
            merge a T = Impl a T
            merge a F = Impl a F
            merge a b@(Not c) = Impl a b
            merge a b@(Var c) = Impl a b
            merge a b@(And c d) = Impl a b
            merge a b@(Or c d) = Impl a b
            merge a (Impl c d) = Impl (Impl a c) d
            merge a (DImpl c d) = DImpl (merge a c) d

parseOr :: Parser Formula
parseOr = do
    a <- parseAtom
    string "\\/"
    b <- parseFormula
    spaces
    return $ merge a b
    where   merge :: Formula -> Formula -> Formula
            merge a T = Or a T
            merge a F = Or a F
            merge a b@(Not c) = Or a b
            merge a b@(Var c) = Or a b
            merge a b@(And c d) = Or a b
            merge a (Or c d) = Or (Or a c) d
            merge a (Impl c d) = Impl (merge a c) d
            merge a (DImpl c d) = DImpl (merge a c) d

parseAnd :: Parser Formula
parseAnd = do
    a <- parseAtom
    string "/\\"
    b <- parseFormula 
    spaces
    return $ merge a b
    where   merge :: Formula -> Formula -> Formula
            merge a T = And a T
            merge a F = And a F
            merge a b@(Not c) = And a b
            merge a b@(Var c) = And a b
            merge a (And c d) = And (And a c) d
            merge a (Or c d) = Or (merge a c) d
            merge a (Impl c d) = Impl (merge a c) d
            merge a (DImpl c d) = DImpl (merge a c) d

parseAtom :: Parser Formula
parseAtom = parseT <|>
            parseF <|>
            parseVar
