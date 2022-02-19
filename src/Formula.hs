module Formula where

type Symb = String 

data Formula = Symb | T | F 
          | Neg Formula    
          | And Formula Formula
          | Or Formula Formula
          | Impl Formula Formula
          | DImpl Formula Formula   
    deriving Eq