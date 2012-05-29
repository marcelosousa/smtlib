{-#LANGUAGE FlexibleContexts #-}

module Parser.Base where

import Data.Char 

import SMTLib2.Base

import Parser.CharSet

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

-- Increase the fixity of *>
infixl 5 **>
p **> q = p *> q

-- digit2Num
digit2Num :: Num a => Char -> a
digit2Num a = fromInteger $ toInteger $ ord a - ord '0'

-- <numeral>
pNumeralStr :: Parser String
pNumeralStr =  (:[]) <$> pSym '0'
           <|> (:) <$> pDigitNonZero <*> pList pDigit
           
pSNumeral :: Num a => Parser a
pSNumeral = foldl (\a b -> a * 10 + (digit2Num b)) 0 <$> pNumeralStr <?> "<numeral>"

-- <decimal>
pSDecimal :: Parser Double
pSDecimal = (\n _ d -> read (n ++ "." ++ d)) <$> pNumeralStr <*> pSym '.' <*> pList pDigit <?> "<decimal>"
  
-- <binary>
pBDigit :: Parser BDigit
pBDigit = pSym '0' <|> pSym '1'

pSBinary :: Parser SBinary
pSBinary = (++) <$> pToken "#b" <*> pList1 pBDigit <?> "<binary>"
  
-- <hex>
pHDigit :: Parser HDigit
pHDigit = pDigit <|> pLower <|> pUpper

pSHex :: Parser SHex
pSHex = (++) <$> pToken "#x" <*> pList1 pHDigit <?> "<hex>"
  
-- <string> TODO
pStrRaw :: Parser String
pStrRaw = pList pChar

pStringRaw :: Parser String 
pStringRaw = (\b s e -> show b ++ s ++ show e) <$> pSym '"' <*> pList pChar <*> pSym '"' <?> "<string>"

pString :: Parser String
pString = lexeme pStringRaw
