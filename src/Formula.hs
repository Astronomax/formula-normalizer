module Formula where

type Symb = String 

data Formula = Var Symb | T | F 
          | Not Formula    
          | And Formula Formula
          | Or Formula Formula
          | Impl Formula Formula
          | DImpl Formula Formula   
    deriving Eq

infix 4 `And`
infix 3 `Or`
infix 2 `Impl`
infix 1 `DImpl`

instance Show Formula where
    showsPrec _ (Var s) = showString s
    showsPrec _ T = showString "1"
    showsPrec _ F = showString "0"       
    showsPrec p (Not f) = showParen (p > 10) $ 
        showString "~" . showsPrec 11 f       
    showsPrec p (And a b) = showParen (p > 4) $ 
        showsPrec 4 a . showString " /\\ " . showsPrec 5 b
    showsPrec p (Or a b) = showParen (p > 3) $ 
        showsPrec 3 a . showString " \\/ " . showsPrec 4 b   
    showsPrec p (Impl a b) = showParen (p > 2) $ 
        showsPrec 2 a . showString " -> " . showsPrec 3 b
    showsPrec p (DImpl a b) = showParen (p > 1) $ 
        showsPrec 1 a . showString " <-> " . showsPrec 2 b