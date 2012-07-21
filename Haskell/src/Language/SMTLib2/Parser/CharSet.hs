{-#LANGUAGE FlexibleContexts #-}

-- | The module "Parser.CharSet" contains parsers for several character sets.
module Language.SMTLib2.Parser.CharSet where

import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU hiding (parse)

-- |'pDigitNonZero' ~=> @[1-9]@
pDigitNonZero :: Parser Char
pDigitNonZero = pRange ('1','9')

-- |'pAlphaNumeric' ~=> @[a-zA-Z0-9]@
pAlphaNumeric :: Parser Char
pAlphaNumeric = pLower <|> pUpper <|> pDigit

-- |'pSpace' ~=> @[ \\r\\n\\t]@
pSpace :: Parser Char
pSpace = pAnySym " \r\n\t" <?> "<single-whitespace>"

-- |'pSpaces1' ~=> @[ \\r\\n\\t]+@
pSpaces1 :: Parser String
pSpaces1 =  pList1 pSpace

-- |'pLineTerm' ~=> @[\\r\\n]@
pLineTerm :: Parser Char
pLineTerm = pAnySym "\r\n"

-- | 'pCharString' parses any @smt-lib@ escaping \\ and \".
pCharString :: Parser Char
pCharString =  pSym '!'
           <|> curry snd <$> pSym '\\' <*> pAnySym "\\\""
           <|> pRange ('\035','\091') 
           <|> pRange ('\093','\126') 
           <|> pSpace

-- | Reserved words.
reswrds :: [String]
reswrds = ["assert","check-sat","declare-fun","declare-sort"
         ,"define-fun","define-sort","exit","get-assertions"
         ,"get-assignment","get-info","get-option","get-proof"
         ,"get-unsat-core","get-value","pop","push","set-info"
         ,"set-logic","set-option","par","NUMERAL","DECIMAL"
         ,"STRING","_","!","as","let","forall","exists"]

-- | Special symbol characters @~!\@$%^&*_-+=\<>.?\/@
symchars :: String
symchars = "~!@$%^&*_-+=<>.?/"

-- | 'pSymChar' ~=> @[a-zA-Z0-9~!\@$%^&*_-+=\<>.?\/]@
pSymChar :: Parser Char
pSymChar = pAlphaNumeric <|> pAnySym symchars

-- | 'pSymCharAlpha' ~=> @[a-zA-Z~!\@$%^&*_-+=\<>.?\/]@
pSymCharAlpha :: Parser Char
pSymCharAlpha = pLower <|> pUpper <|> pAnySym symchars

-- | 'pCharQSym' parses a quote symbol character.
pCharQSym :: Parser Char
pCharQSym = pSpace <|> pRange ('\033','\091') <|> pRange ('\093','\123') <|> pRange ('\125','\126')

-- | 'pChar' parses a @smt-lib@ character.
pChar :: Parser Char
pChar = pRange ('\033','\126') <|> pSpace


