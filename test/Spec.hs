{-# LANGUAGE InstanceSigs #-}

import Formula
import Parser (parseFormula)
import FormulaToNNF (formulaToNNF, isInNNF)
import FormulaToDNF (formulaToDNF, isInDNF)
import FormulaToCNF (formulaToCNF, isInCNF)
import Text.Parsec
import Test.QuickCheck
import Control.Monad
import Data.Either

newtype Name = Name { getName :: String }

instance Arbitrary Name where
    arbitrary :: Gen Name
    arbitrary = sized genName
        where genName :: Int -> (Gen Name)
              genName n = Name <$> replicateM (n + 1) (oneof $ return <$> alphabet)
              alphabet = (take 26 $ iterate succ 'a') ++ (take 26 $ iterate succ 'A')

instance Arbitrary Formula where
    arbitrary :: Gen Formula
    arbitrary = sized genFormula
        where genFormula :: Int -> Gen Formula
              genFormula 0 = oneof [ Var <$> (getName <$> arbitrary),
                                     return T,
                                     return F ]
              genFormula n = frequency  [ (3,   (genFormula 0)),
                                          (1,   Not <$> genFormula (n - 1)),
                                          (4,   let l = choose (0, n)
                                                    r = (-) <$> (return $ n - 1) <*> l in
                                                    oneof [ And <$> (l >>= genFormula) <*> (r >>= genFormula),
                                                            Or <$> (l >>= genFormula) <*> (r >>= genFormula),
                                                            Impl <$> (l >>= genFormula) <*> (r >>= genFormula),
                                                            DImpl <$> (l >>= genFormula) <*> (r >>= genFormula) ]) ]

prop_ParsePrint f = let     s = show f :: String
                            p = parseFormula s :: Either ParseError Formula
                            (Right d) = p
                            in d == f
    where types = f::Formula

prop_NNF f = isInNNF $ formulaToNNF f
prop_DNF f = formulaLength (formulaToNNF f) < 100 ==> isInDNF $ formulaToDNF f
prop_CNF f = formulaLength (formulaToNNF f) < 100 ==> isInCNF $ formulaToCNF f
    
formulaLength :: Formula -> Int 
formulaLength (Not a) = 1 + (formulaLength a)
formulaLength (And a b) = 1 + (formulaLength a) + (formulaLength b)
formulaLength (Or a b) = 1 + (formulaLength a) + (formulaLength b)
formulaLength (Impl a b) = 1 + (formulaLength a) + (formulaLength b)
formulaLength (DImpl a b) = 1 + (formulaLength a) + (formulaLength b)
formulaLength f = 0

main :: IO ()
main = do
    quickCheck prop_ParsePrint
    quickCheck prop_NNF
    quickCheck prop_DNF
    quickCheck prop_CNF
