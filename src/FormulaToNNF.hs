{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormulaToNNF (formulaToNNF, isInNNF) where

import Formula

formulaToNNF :: Formula -> Formula
formulaToNNF = siftDown . eliminateImpls
    where   siftDown :: Formula -> Formula
            siftDown (Not (Not a)) = siftDown a
            siftDown (Not (And a b)) = Or (siftDown (Not a)) (siftDown (Not b)) 
            siftDown (Not (Or a b)) = And (siftDown (Not a)) (siftDown (Not b))
            siftDown (And a b) = And (siftDown a) (siftDown b)
            siftDown (Or a b) = Or (siftDown a) (siftDown b)
            siftDown f = f

            eliminateImpls :: Formula -> Formula
            eliminateImpls (Not a) = Not $ eliminateImpls a
            eliminateImpls (And a b) = And (eliminateImpls a) (eliminateImpls b)
            eliminateImpls (Or a b) = Or (eliminateImpls a) (eliminateImpls b)
            eliminateImpls (Impl a b) = eliminateImpls $ Or (Not a) b
            eliminateImpls (DImpl a b) = eliminateImpls $ And (Impl a b) (Impl b a)
            eliminateImpls f = f

isInNNF :: Formula -> Bool
isInNNF (Var a) = True
isInNNF T = True
isInNNF F = True
isInNNF (Not (Var s)) = True
isInNNF (Not T) = True
isInNNF (Not F) = True
isInNNF (a `And` b) = (isInNNF a) && (isInNNF b)
isInNNF (a `Or` b) = (isInNNF a) && (isInNNF b)
isInNNF _ = False

