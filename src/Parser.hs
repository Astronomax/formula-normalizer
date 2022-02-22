{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser (parseFormula) where

import Formula
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef {    identStart = letter, 
                    identLetter = alphaNum, 
                    opStart = oneOf ['~', '/', '\\', '<', '-'], 
                    opLetter = oneOf ['~', '/', '\\', '<', '>', '-'], 
                    reservedOpNames = ["~", "/\\", "\\/", "->", "<->"], 
                    reservedNames = ["1", "0"] }

TokenParser {   parens = m_parens, 
                identifier = m_identifier, 
                reservedOp = m_reservedOp, 
                reserved = m_reserved, 
                semiSep1 = m_semiSep1, 
                whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Formula
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return Not)],
          [Infix (m_reservedOp "/\\" >> return And) AssocLeft],
          [Infix (m_reservedOp "\\/" >> return Or) AssocLeft],
          [Infix (m_reservedOp "->" >> return Impl) AssocLeft],
          [Infix (m_reservedOp "<->" >> return DImpl) AssocLeft] ]

term = m_parens exprparser <|> 
    fmap Var m_identifier <|> 
    (m_reserved "1" >> return T) <|> 
    (m_reserved "0" >> return F)

mainparser :: Parser Formula
mainparser = m_whiteSpace >> exprparser <* eof

parseFormula :: String -> Either ParseError Formula
parseFormula = parse mainparser ""