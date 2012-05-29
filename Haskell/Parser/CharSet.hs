{-#LANGUAGE FlexibleContexts #-}

module Parser.CharSet where

import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU hiding (parse)

-- Character Sets

-- 'digits' is already defined in the UU lib
-- 'digits non zero' 
pDigitNonZero :: Parser Char
pDigitNonZero = pRange ('1','9')

-- 'alphanumeric'
pAlphaNumeric :: Parser Char
pAlphaNumeric = pLower <|> pUpper <|> pDigit

-- 'whitespace'
whitespace :: String
whitespace = " \r\n\t"

pSpace :: Parser Char
pSpace = pAnySym whitespace

pSpaces' :: Parser String
pSpaces' = pList1 pSpace

-- 'line terminators'
lineterm :: String
lineterm = "\r\n"

pLineTerm :: Parser Char
pLineTerm = pAnySym lineterm

-- 'symbol characters'
symchars :: String
symchars = "~!@$%^&*_-+=<>.?"

pSymChar :: Parser Char
pSymChar = pAlphaNumeric <|> pAnySym symchars

pSymCharAlpha :: Parser Char
pSymCharAlpha = pLower <|> pUpper <|> pAnySym symchars

-- 'smt-lib character'
pChar :: Parser Char
pChar = pRange ('\033','\126') <|> pSpace

-- characters for quoted symbols
pCharQSym :: Parser Char
pCharQSym = pSpace <|> pRange ('\033','\091') <|> pRange ('\093','\123') <|> pRange ('\125','\126')

