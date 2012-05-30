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
pSBinary = (++) <$> pToken "#b" <*> pList1 pBDigit <?> "<binary>"
  
-- | 'pHDigit' ~=> @[0-9a-fA-F]@
pHDigit :: Parser HDigit
pHDigit = pDigit <|> pRange ('a','f') <|> pRange ('A','F')

-- | 'pSHex' ~=> @#x[0-9a-fA-F]@. We consider <hex> as a string.
pSHex :: Parser SHex
pSHex = (++) <$> pToken "#x" <*> pList1 pHDigit <?> "<hex>"
  
-- | 'pString' parses a sequence of 'pCharString' enclosed in \".
pString :: Parser String 
pString = (\b s e -> show b ++ s ++ show e) <$> pSym '"' <*> pList pCharString <*> pSym '"' <?> "<string>"

-- | 'pSSymbol' parses a 'SSymbol'.
pSSymbol :: Parser SSymbol
pSSymbol =  SimpleSym <$> pSymSimple'  
        <|> QuotedSym <$> pSymQuoted          
        <?> "<symbol>"

cmdNames :: [String]
cmdNames = ["assert","check-sat","declare-fun","declare-sort"
         ,"define-fun","define-sort","exit","get-assertions"
         ,"get-assignment","get-info","get-option","get-proof"
         ,"get-unsat-core","get-value","pop","push","set-info"
         ,"set-logic","set-option"]

--reswrd :: [(String, SResWrd)]
--reswrd = [("par"     , RWpar     )
--         ,("NUMERAL" , RWNUMERAL )     
--         ,("DECIMAL" , RWDECIMAL )     
--         ,("STRING"  , RWSTRING  )     
--         ,("_"       , RW_       )     
--         ,("!"       , RWExclMark)     
--         ,("as"      , RWas      )     
--         ,("let"     , RWlet     )     
--         ,("forall"  , RWforall  )     
--         ,("exists"  , RWexists  )]   

reswrds :: [String]
reswrds = cmdNames -- ++ (fst $ unzip reswrd)

-- <simple-symbol> 
pSymSimple' :: Parser SimpleSym
pSymSimple' = (:) <$> pSymCharAlpha <*> pList pSymChar

pSymSimple :: Parser SimpleSym
pSymSimple = do x <- pSymSimple'
                if elem x reswrds 
                then pFail 
                else pReturn x

-- | 'pSymQuoted' parses a sequence of smt-lib characters non including \\ and \|, inclosed in \| characters.
pSymQuoted :: Parser QuotedSym
pSymQuoted = pSym '|' *> pList pCharQSym <* pSym '|'

-- | 'pSKeyword' parses a colon followed by a non-empty sequence of symbol characters.
pSKeyword :: Parser SKeyword
pSKeyword = Keyword <$> pSym ':' **> pList1 pSymChar
