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
          emptyParser = parser $ \s -> Just (s, "")

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

parseNeg :: Parser Formula
parseNeg =  (do
                string "~"
                a <- parseInBrackets
                return $ Neg a) <|> 
            (do
                string "~"
                a <- parseWithoutBrackets
                return $ merge a)
                where   merge :: Formula -> Formula
                        merge T = Neg T
                        merge F = Neg F
                        merge (Neg b) = Neg (merge b)
                        merge a@(Var b) = Neg a
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
            merge a b@(Neg c) = DImpl a b
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
            merge a b@(Neg c) = Impl a b
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
            merge a b@(Neg c) = Or a b
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
            merge a b@(Neg c) = And a b
            merge a b@(Var c) = And a b
            merge a (And c d) = And (And a c) d
            merge a (Or c d) = Or (merge a c) d
            merge a (Impl c d) = Impl (merge a c) d
            merge a (DImpl c d) = DImpl (merge a c) d

parseAtom :: Parser Formula
parseAtom = parseT <|>
            parseF <|>
            parseVar

-- https://stackoverflow.com/questions/27471937/showsprec-and-operator-precedences

{-
instance Show Type where
    showsPrec _ (TVar s)  = showString s

    showsPrec p (a :-> b) = showParen (p > 3) $ 
        showsPrec 4 a . showString " -> " . showsPrec 3 b

showTypeAtom :: Type -> ShowS
showTypeAtom (TVar s) = showString s
showTypeAtom t        = showParen True $ shows t

instance Show Expr where
    showsPrec _ (Var s)     = showString s

    showsPrec p (a :@ b)    = showParen (p > 4) $ 
        showsPrec 4 a . showChar ' ' . showsPrec 5 b

    showsPrec p (Lam s t e) = showParen (p > 0) $ 
        showChar '\\' . showString s . showString " : " 
        . showTypeAtom t . showString ". " . shows e

instance Show Env where
    showsPrec _ (Env [])      = showString ""

    showsPrec _ (Env (d:ds))  =
        foldl' appendDecl (showDecl d) ds
        where appendDecl sh d' =
                sh . showString ", " . showDecl d'
              showDecl (s, t) = 
                showString s . showString " : " . shows t

instance Show TypingRelation where
    showsPrec p (TypingRelation env expr t) = 
        showParen (p > 0) $ shows env . showVdash env 
        . shows expr . showString " : " . shows t
        where showVdash (Env e) = if null e
                                  then showString "|- "
                                  else showString " |- "

instance Show TypeError where
    show (FreeVarNotTyped env x) =
        "The type of the free variable " ++ show x
        ++ " is not given in the environment:\n"
        ++ "    " ++ show env
    show (LeftApplicantIsNotArrow env expr ty) =
        "The type of the left applicant is not an arrow type:\n"
        ++ "    " ++ show (TypingRelation env expr ty)
    show (ApplicationTypesMismatch env (e1, t1) (e2, t2)) =
        "The types of the applicants does not match:\n"
        ++ "    " ++ show (TypingRelation env e1 t1) ++ "\n"
        ++ "    " ++ show (TypingRelation env e2 t2)
    show (InferredAndGivenTypesMismatch rel inferred) =
        "The given typing relation is:\n"
        ++ "    " ++ show rel ++ "\n"
        ++ "but the inferred type is:\n"
        ++ "    " ++ show inferred
-}