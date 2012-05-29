{-#LANGUAGE FlexibleContexts #-}

module Parser.Base where

import Data.Char 

import SMTLib2.Base

import Parser.CharSet

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

infixl 5 **>
p **> q = p *> q

-- digit
digit2Int :: Num a => Char -> a
digit2Int a = fromInteger $ toInteger $ ord a - ord '0'

-- <numeral>
pNumeralStr :: Parser String
pNumeralStr = (:[]) <$> pSym '0'
           <|> (:) <$> pDigitNonZero <*> pList pDigit
           
pNumeralRaw :: Num a => Parser a
pNumeralRaw = foldl (\a b -> a * 10 + (digit2Int b)) 0 <$> pNumeralStr <?> "<numeral>"

pSNumeral :: Num a => Parser a
pSNumeral = lexeme pNumeralRaw

-- <decimal>
pDecimalRaw :: Parser Double
pDecimalRaw = (\b p e -> read (b ++ "." ++ e)) <$> pNumeralStr <*> pSym '.' <*> pList pDigit <?> "<decimal>"
  
pSDecimal :: Parser Double
pSDecimal = lexeme pDecimalRaw

-- <binary>
pBDigit :: Parser BDigit
pBDigit = pSym '0' <|> pSym '1'

pBinaryRaw :: Parser SBinary
pBinaryRaw = (++) <$> pToken "#b" <*> pList1 pBDigit <?> "<binary>"
  
pSBinary :: Parser SBinary
pSBinary = lexeme pBinaryRaw

-- <hex>
pHDigit :: Parser HDigit
pHDigit = pDigit <|> pLower <|> pUpper

pHexRaw :: Parser SHex
pHexRaw = (++) <$> pToken "#x" <*> pList1 pHDigit <?> "<hex>"
  
pSHex :: Parser SHex
pSHex = lexeme pHexRaw

-- <string> TODO
pStrRaw :: Parser String
pStrRaw = pList pChar

pStringRaw :: Parser String 
pStringRaw = (\b s e -> show b ++ s ++ show e) <$> pSym '"' <*> pList pChar <*> pSym '"' <?> "<string>"

pString :: Parser String
pString = lexeme pStringRaw
