{-#LANGUAGE FlexibleContexts #-}

-- | The module "Parser.Token" contains parsers for tokens.
module Parser.Token where

import Data.Char 

import SMTLib2.Base

import Parser.CharSet

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

infixl 5 **>

-- | '**>' is a version of '*>' with increased fixity.
(**>) :: Applicative f => f a -> f b -> f b
p **> q = p *> q

-- digit2Num converts a char to a num.
digit2Num :: Num a => Char -> a
digit2Num a = fromInteger $ toInteger $ ord a - ord '0'

-- | 'pNumeralStr' ~=> @(0|([1-9][0-9]*))@ 
pNumeralStr :: Parser String
pNumeralStr =  (:[]) <$> pSym '0'
           <|> (:)   <$> pDigitNonZero <*> pList pDigit

-- | 'pSNumeral' converts a string form of a <numeral> into a Num @a@.
pSNumeral :: Num a => Parser a
pSNumeral = foldl (\a b -> a * 10 + (digit2Num b)) 0 <$> pNumeralStr <?> "<numeral>"

-- | 'pSDecimal' ~=> @(0|([1-9][0-9]*))[.]([0-9]+)@.
pSDecimal :: Parser Double
pSDecimal = (\n _ d -> read (n ++ "." ++ d)) <$> pNumeralStr <*> pSym '.' <*> pList1 pDigit <?> "<decimal>"
  
-- | 'pBDigit' ~=> @[01]@.
pBDigit :: Parser BDigit
pBDigit = pSym '0' <|> pSym '1'

-- | 'pSBinary' ~=> @#b[01]+@. We consider <binary> as a string.
pSBinary :: Parser SBinary
--pSBinary = (++) <$> pToken "0b" <*> pList1 pBDigit <?> "<binary>"
pSBinary = pToken "0b" **> pList1 pBDigit <?> "<binary>"

-- | 'pHDigit' ~=> @[0-9a-fA-F]@
pHDigit :: Parser HDigit
pHDigit = pDigit <|> pRange ('a','f') <|> pRange ('A','F')

-- | 'pSHex' ~=> @#x[0-9a-fA-F]@. We consider <hex> as a string.
pSHex :: Parser SHex
--pSHex = (++) <$> pToken "0x" <*> pList1 pHDigit <?> "<hex>"
pSHex = pToken "0x" **> pList1 pHDigit <?> "<hex>"
  
-- | 'pString' parses a sequence of 'pCharString' enclosed in \".
pString :: Parser String 
--pString = (\b s e -> show b ++ s ++ show e) <$> pSym '"' <*> pList pCharString <*> pSym '"' <?> "<string>"
pString = (\b s e -> s) <$> pSym '"' <*> pList pCharString <*> pSym '"' <?> "<string>"
 
-- | 'pSLiteral' parses any literal.
pSLiteral :: Parser SLiteral
pSLiteral =  NumLit <$> pSNumeral
         <|> DecLit <$> pSDecimal
         <|> BinLit <$> pSBinary
         <|> HexLit <$> pSHex
         <|> StrLit <$> pString
         <?> "<literal>"

--pSReserved :: Parser SReserved
--pSReserved =  ResWrd <$> pSResWord
--          <|> Cmd    <$> pSCmd
--          <?> "<reserved>"
--          
--pSResWord :: Parser SResWrd
--pSResWord =  pAny (uncurry liftToken) reswrd
--         <?> "<reserved-not-command>"


-- | 'pSSymbol' parses a 'SSymbol'.
pSSymbol :: Parser SSymbol
pSSymbol =  SimpleSym <$> pSimpleSym
        <|> QuotedSym <$> pQuotedSym
        <|> ReservSym <$> pReservSym
        <?> "<symbol>"

-- | 'pReservSym' parses a reserved word.
pReservSym :: Parser ReservSym
pReservSym = pAny pToken reswrds `micro` 1

-- | 'pSymSimple' parses a non-empty sequence of symbol characters, not beginning with a digit, and not a reserved word.
pSimpleSym :: Parser SimpleSym
pSimpleSym = (:) <$> pSymCharAlpha <*> pList pSymChar `micro` 2
-- do x <- (:) <$> pSymCharAlpha <*> pList pSymChar
--                if elem x reswrds 
--                then pFail
--                else pReturn x

-- | 'pSymQuoted' parses a sequence of smt-lib characters non including \\ and \|, inclosed in \| characters.
pQuotedSym :: Parser QuotedSym
pQuotedSym = pSym '|' *> pList pCharQSym <* pSym '|'

-- | 'pSKeyword' parses a colon followed by a non-empty sequence of symbol characters.
pSKeyword :: Parser SKeyword
pSKeyword = Keyword <$> pSym ':' **> pList1 pSymChar

-- -- <token>
-- pSToken :: Parser SToken
-- pSToken =  LitToken <$> pSLiteral
--        <|> ResToken <$> pSReserved
-- --       <|> SymToken <$> pSSymbol
--        <|> KeyToken <$> pSKeyword
--        <?> "<token>"
