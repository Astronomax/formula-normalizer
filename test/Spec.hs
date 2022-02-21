{-# LANGUAGE InstanceSigs #-}

import Test.QuickCheck
import Formula
import Control.Monad

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


-- var name is not empty
prop_Formula (Var s) = (length s) > 0
prop_Formula T = True
prop_Formula F = True
prop_Formula (Not f) = prop_Formula f
prop_Formula (And a b) = (prop_Formula a) && (prop_Formula b)
prop_Formula (Or a b) = (prop_Formula a) && (prop_Formula b)
prop_Formula (Impl a b) = (prop_Formula a) && (prop_Formula b)
prop_Formula (DImpl a b) = (prop_Formula a) && (prop_Formula b)

main :: IO ()
main = quickCheck prop_Formula
