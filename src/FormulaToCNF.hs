module FormulaToCNF where

import Formula
import FormulaToNNF (formulaToNNF)
import FormulaToDNF (formulaToDNF)

formulaToCNF :: Formula -> Formula
formulaToCNF = swapAndOr . formulaToDNF . swapAndOr . formulaToNNF
    where   swapAndOr :: Formula -> Formula
            swapAndOr (Not a) = Not (swapAndOr a)
            swapAndOr (And a b) = Or (swapAndOr a) (swapAndOr b)
            swapAndOr (Or a b) = And (swapAndOr a) (swapAndOr b)
            swapAndOr f = f

isInCNF :: Formula -> Bool
isInCNF (a `And` b) = (isInCNF a) && (isInCNF b)
isInCNF a = isCNFClause a
    where   isCNFClause :: Formula -> Bool
            isCNFClause (Or a b) = (isCNFClause a) && (isCNFClause b)
            isCNFClause (And a b) = False
            isCNFClause _ = True