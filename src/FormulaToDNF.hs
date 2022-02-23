{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormulaToDNF (formulaToDNF, isInDNF) where

import Formula
import FormulaToNNF (formulaToNNF)

formulaToDNF :: Formula -> Formula
formulaToDNF = (nnfToDNF) . formulaToNNF
    where
          nnfToDNF :: Formula -> Formula
          nnfToDNF (And a b) = conjDNFs (nnfToDNF a) (nnfToDNF b)
          nnfToDNF (Or a b) = Or (nnfToDNF a) (nnfToDNF b)
          nnfToDNF f = f
          
          conjDNFs :: Formula -> Formula -> Formula
          conjDNFs (Or a b) c = Or (conjDNFs a c) (conjDNFs b c)
          conjDNFs a (Or b c) = Or (conjDNFs a b) (conjDNFs a c)
          conjDNFs a b = And a b

isInDNF :: Formula -> Bool
isInDNF (Or a b) = (isInDNF a) && (isInDNF b)
isInDNF a = isDNFClause a
    where   isDNFClause :: Formula -> Bool
            isDNFClause (And a b) = (isDNFClause a) && (isDNFClause b)
            isDNFClause (Or a b) = False
            isDNFClause _ = True