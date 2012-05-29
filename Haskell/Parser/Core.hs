{-#LANGUAGE FlexibleContexts,RankNTypes,GADTs #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE EmptyDataDecls     #-}

module Parser.Core where
  
import SMTLib2.Base

import Parser.Base
import Parser.CharSet

import Control.Monad

import Text.ParserCombinators.UU hiding (parse, (<$$>))
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Demo.Examples (run)

liftToken :: String -> a -> Parser a
liftToken s c = const c <$> pToken s

pSMod :: Parser SMod
pSMod = pList1 pSExpression <?> "<module>"

-- Take out comments
ptSExpr :: ParserTrafo a a
ptSExpr p =  pSpaces *> pLParen *> pSpaces *> p <* pSpaces <* pRParen <* pSpaces

ptSExpr' :: ParserTrafo a a
ptSExpr' p =  pSpaces' *> pLParen *> pSpaces *> p <* pSpaces <* pRParen <* pSpaces'
         
-- <S-expr> 
pSExpression :: Parser SExpression
pSExpression =  Token    <$> pSToken 
            <|> SeqSExpr <$> ptSExpr (pList pSExpression) 
            <?> "<S-expr>"

-- <token>
pSToken :: Parser SToken
pSToken =  LitToken <$> pSLiteral
       <|> ResToken <$> pSReserved
--       <|> SymToken <$> pSSymbol
       <|> KeyToken <$> pSKeyword
       <?> "<token>"

-- <literal>
pSLiteral :: Parser SLiteral
pSLiteral =  NumLit <$> pSNumeral
         <|> DecLit <$> pSDecimal
         <|> BinLit <$> pSBinary
         <|> HexLit <$> pSHex
         <|> StrLit <$> pString
         <?> "<literal>"

cmdNames :: [String]
cmdNames = ["assert","check-sat","declare-fun","declare-sort"
         ,"define-fun","define-sort","exit","get-assertions"
         ,"get-assignment","get-info","get-option","get-proof"
         ,"get-unsat-core","get-value","pop","push","set-info"
         ,"set-logic","set-option"]

reswrd :: [(String, SResWrd)]
reswrd = [("par"     , RWpar     )
         ,("NUMERAL" , RWNUMERAL )     
         ,("DECIMAL" , RWDECIMAL )     
         ,("STRING"  , RWSTRING  )     
         ,("_"       , RW_       )     
         ,("!"       , RWExclMark)     
         ,("as"      , RWas      )     
         ,("let"     , RWlet     )     
         ,("forall"  , RWforall  )     
         ,("exists"  , RWexists  )]   

reswrds :: [String]
reswrds = cmdNames ++ (fst $ unzip reswrd)

pSReserved :: Parser SReserved
pSReserved =  ResWrd <$> pSResWord
          <|> Cmd    <$> pSCmd
          <?> "<reserved>"
          
pSResWord :: Parser SResWrd
pSResWord =  pAny (uncurry liftToken) reswrd
         <?> "<reserved-not-command>"

app :: (c -> d) -> a -> b -> c -> d
app f _ _ c = f c

-- * SMT Commands *
-- SMT Commands without arguments
scmd0arg = [("get-assertions", GetAsserts)
           ,("check-sat"     , CheckSat)
           ,("get-proof"     , GetProof)
           ,("get-unsat-core", GetUnsatCore)
           ,("get-assignment", GetAssign)
           ,("exit"          , Exit)]

liftCmd :: (a -> b) -> String -> Parser a -> Parser b
liftCmd con scon pb = con <$> pToken scon **> pSpaces' **> pb

pSCmd :: Parser SCmd
pSCmd =  pAny (uncurry liftToken) scmd0arg 
     <|> SetLogic <$> pToken "set-logic"    **> pSpaces' **> pSLogic
     <|> Assert   <$> pToken "assert"       **> pSpaces' **> pSExpr
     <|> GetValue <$> pToken "get-value"    **> pSpaces' **> ptSExpr (pList1 pSExpr)
     <|> Pop      <$> pToken "pop"          **> pSpaces' **> pSNumeral
     <|> Push     <$> pToken "push"         **> pSpaces' **> pSNumeral
     <|> GetOpt   <$> pToken "get-option"   **> pSpaces' **> pSKeyword
     <|> SetOpt   <$> pToken "set-option"   **> pSpaces' **> pSKeyword <*> pSpaces' **> pSAttrValue
     <|> GetInfo  <$> pToken "get-info"     **> pSpaces' **> pSKeyword
     <|> SetInfo  <$> pToken "set-info"     **> pSpaces' **> pSKeyword <*> pSpaces' **> pSAttrValue
     <|> DeclFun  <$> pToken "declare-fun"  **> pSpaces' **> pSSymbol <*> ptSExpr (pList pSSortExpr) <*> pSSortExpr
     <|> DefFun   <$> pToken "define-fun"   **> pSpaces' **> pSSymbol <*> ptSExpr (ptSExpr $ pList $ (,) <$> pSSymbol <*> pSSortExpr) <*> pSSortExpr <*> pSpaces' **> pSExpr
     <|> DeclSort <$> pToken "declare-sort" **> pSpaces' **> pSSymbol <*> pSpaces' **> pSNumeral
     <|> DefSort  <$> pToken "define-sort"  **> pSpaces' **> pSSymbol <*> ptSExpr (pList1 pSSymbol) <*> pSExpr
     <?> "<command>"

-- pLogic
pSLogic :: Parser SLogic
pSLogic = read <$> pList1 pSymChar
 
pSExpr :: Parser SExpr
pSExpr =  LitExpr    <$> pSLiteral
      <|> IdentExpr  <$> pSIdent
      <|> FnAppExpr  <$> pSIdent <* pSpaces <*> pList1 pSExpr
      <|> ForallExpr <$> pToken "forall" **> ptSExpr (ptSExpr $ pList1 $ (,) <$> pSSymbol <*> pSSort) <*> pSpaces **> pSExpr
      <|> ExistsExpr <$> pToken "exists" **> ptSExpr (ptSExpr $ pList1 $ (,) <$> pSSymbol <*> pSSort) <*> pSpaces **> pSExpr
      <|> LetExpr    <$> pToken "let"    **> ptSExpr (ptSExpr $ pList1 $ (,) <$> pSSymbol <*> pSSort) <*> pSpaces **> pSExpr
      <|> AttrExpr   <$> pToken "!"      **> pSpaces **> pSExpr <*> pList1 (pSpaces **> pSAttribute)
      <?> "<expr>" 
     
pSAttribute :: Parser SAttribute
pSAttribute =  AttrKey      <$> pSKeyword
           <|> AttrKeyValue <$> pSKeyword <*> pSpaces **> pSAttrValue

pSAttrValue :: Parser SAttrValue
pSAttrValue = pList1 pSymChar

pSIdent :: Parser SIdent
pSIdent =  SymIdent <$> pSSymbol
       <|> IdxIdent <$> pToken "_" **> pSpaces **> pSSymbol <*> pList1 (pSpaces **> pSNumeral)
       <|> QlfIdent <$> pToken "as" **> pSpaces **> pSIdent <*> pSpaces **> pSSort
       <?> "<identifier>"
               
pSSort :: Parser SSort
pSSort = Sort <$> pList1 pSymChar <?> "<sort>"

pSSortExpr :: Parser SSortExpr
pSSortExpr =  SymSort <$> pSSort
          <|> ptSExpr (FunSort <$> pSSymbol <*> pSpaces **> pList1 pSSortExpr)
          <?> "<sort-expr>"
          
-- <symbol>
pSSymbol :: Parser SSymbol
pSSymbol =  SimpleSym <$> pSymSimple'  
        <|> QuotedSym <$> pSymQuoted          
        <?> "<symbol>"

-- <simple-symbol> 
pSymSimple' :: Parser SimpleSym
pSymSimple' = (:) <$> pSymCharAlpha <*> pList pSymChar

pSymSimple :: Parser SimpleSym
pSymSimple = do x <- pSymSimple'
                if elem x reswrds 
                then pFail 
                else pReturn x

-- <quoted-symbol>
pSymQuoted :: Parser QuotedSym
pSymQuoted = pSym '|' *> pList pCharQSym <* pSym '|'

-- <keyword>
pSKeyword :: Parser SKeyword
pSKeyword = Keyword <$> pSym ':' **> pList1 pSymChar

          