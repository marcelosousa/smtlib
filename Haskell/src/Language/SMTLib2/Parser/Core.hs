{-#LANGUAGE FlexibleContexts,RankNTypes,GADTs #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE EmptyDataDecls     #-}

module Language.SMTLib2.Parser.Core where
  
import Language.SMTLib2.Base

import Language.SMTLib2.Parser.Token
import Language.SMTLib2.Parser.CharSet

import Control.Monad
import Debug.Trace

import Text.ParserCombinators.UU hiding (parse, (<$$>))
import Text.ParserCombinators.UU.Utils hiding (pLParen, pRParen)
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Demo.Examples (run)

liftToken :: String -> a -> Parser a
liftToken s c = const c <$> pToken s  

pSMod :: Parser SMod
--pSMod = pList1 pSExpression <?> "<module>"
pSMod =  pList1 (SE <$> ptSExpr pSCmd <|> const Comment <$> pSpaces **> pCmt) <?> "<module>" 

pLParen, pRParen :: Parser Char
pLParen = pSym '('
pRParen = pSym ')'

ptSExpr :: ParserTrafo a a
ptSExpr p =  pSpaces *> pLParen *> pSpaces *> p <* pSpaces <* pRParen <* pSpaces <* pMaybe pCmt

pEncSExpr :: ParserTrafo a a
pEncSExpr p = pLParen *> pSpaces *> p <* pSpaces <* pRParen

-- <S-expr> 
-- pSExpression :: Parser SExpression
-- pSExpression =  Token    <$> pSToken 
--             <|> SeqSExpr <$> ptSExpr (pList pSExpression) 
--             <?> "<S-expr>"
-- 

-- * SMT Commands *
-- SMT Commands without arguments
scmd0arg = [("get-assertions", GetAsserts)
           ,("check-sat"     , CheckSat)
           ,("get-proof"     , GetProof)
           ,("get-unsat-core", GetUnsatCore)
           ,("get-assignment", GetAssign)
           ,("exit"          , Exit)]

liftCmd :: (a -> b) -> String -> Parser a -> Parser b
liftCmd con scon pb = con <$> pToken scon **> pSpaces1 **> pb

pSCmd :: Parser SCmd
pSCmd =  pAny (uncurry liftToken) scmd0arg 
     <|> SetLogic <$> pToken "set-logic"    **> pSpaces1 **> pSLogic
     <|> Assert   <$> pToken "assert"       **> pSpaces1 **> pSExpr
     <|> GetValue <$> pToken "get-value"    **> pSpaces1 **> ptSExpr (pList1 pSExpr)
     <|> Pop      <$> pToken "pop"          **> pSpaces1 **> pSNumeral
     <|> Push     <$> pToken "push"         **> pSpaces1 **> pSNumeral
     <|> GetOpt   <$> pToken "get-option"   **> pSpaces1 **> pSKeyword
     <|> SetOpt   <$> pToken "set-option"   **> pSpaces1 **> pSKeyword <*> pSpaces1 **> pSAttrValue
     <|> GetInfo  <$> pToken "get-info"     **> pSpaces1 **> pSKeyword
     <|> SetInfo  <$> pToken "set-info"     **> pSpaces1 **> pSKeyword <*> pSpaces1 **> pSAttrValue
     <|> DeclFun  <$> pToken "declare-fun"  **> pSpaces1 **> pSSymbol <*> ptSExpr (pList pSSortExpr) <*> pSSortExpr
     <|> DefFun   <$> pToken "define-fun"   **> pSpaces1 **> pSSymbol <*> ptSExpr (ptSExpr $ pList $ (,) <$> pSSymbol <*> pSSortExpr) <*> pSSortExpr <*> pSpaces1 **> pSExpr
     <|> DeclSort <$> pToken "declare-sort" **> pSpaces1 **> pSSymbol <*> pSpaces1 **> pSNumeral
     <|> DefSort  <$> pToken "define-sort"  **> pSpaces1 **> pSSymbol <*> ptSExpr (pList1 pSSymbol) <*> pSExpr
--     <|> const Comment  <$> pCmt
     <?> "<command>"

-- pLogic
pSLogic :: Parser SLogic
pSLogic = read <$> pList1 pSymChar

pSExpr :: Parser SExpr
pSExpr =  LitExpr    <$> pSLiteral
      <|> IdentExpr  <$> pSIdent
      <|> pEncSExpr (FnAppExpr  <$> pSIdent <*> pList1 (pSpaces1 **> pSExpr))
      <|> pEncSExpr (ForallExpr <$> pToken "forall" **> pSpaces1 **> ptSExpr (pList1 $ ptSExpr $ (,) <$> pSSymbol <*> pSpaces1 **> pSSort) <*> pSExpr)
      <|> pEncSExpr (ExistsExpr <$> pToken "exists" **> pSpaces1 **> ptSExpr (pList1 $ ptSExpr $ (,) <$> pSSymbol <*> pSpaces1 **> pSSort) <*> pSExpr)
      <|> pEncSExpr (LetExpr    <$> pToken "let"    **> pSpaces1 **> ptSExpr (pList1 $ ptSExpr $ (,) <$> pSSymbol <*> pSpaces1 **> pSExpr) <*> pSExpr)
      <|> pEncSExpr (AttrExpr   <$> pToken "!"      **> pSpaces1 **> pSExpr <*> pList1 (pSpaces1 **> pSAttribute))
      <?> "<expr>" 
  
pSAttribute :: Parser SAttribute
pSAttribute =  AttrKey      <$> pSKeyword
           <|> AttrKeyValue <$> pSKeyword <*> pSpaces1 **> pSAttrValue

pSAttrValue :: Parser SExpr
pSAttrValue = pSExpr

pSIdent :: Parser SIdent
pSIdent =  SymIdent <$> pSSymbol
       <|> pEncSExpr (IdxIdent <$> pToken "_"  **> pSpaces1 **> pSSymbol <*> pList1 (pSpaces1 **> pSNumeral))
       <|> pEncSExpr (QlfIdent <$> pToken "as" **> pSpaces1 **> pSIdent  <*> pSpaces1 **> pSSort)
       <?> "<identifier>"
               
pSSort :: Parser SSort
pSSort = pList1 pSymChar <?> "<sort>"

pSSortExpr :: Parser SSortExpr
pSSortExpr =  SymSort <$> pSSort
          <|> pEncSExpr (FunSort <$> pSSymbol <*> pSpaces1 **> pList1 pSSortExpr)
          <?> "<sort-expr>"
          
          
