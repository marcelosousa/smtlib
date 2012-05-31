{-#LANGUAGE FlexibleInstances #-}

module Printer.PPrinter where

import SMTLib2.Base

import UU.PPrint

class SMTPretty a where
  spretty :: a -> Doc

instance (Pretty a, Pretty b) => SMTPretty (a, b) where
  spretty (a,b) = parens $ pretty a <+> pretty b

instance SMTPretty [(SSymbol, SSortExpr)] where
  spretty = hsep . map spretty

instance SMTPretty [(SSymbol, SExpr)] where
  spretty = hsep . map spretty

instance SMTPretty [(SSymbol, SSort)] where
  spretty = hsep . map spretty
  
instance SMTPretty [SSortExpr] where
  spretty = hsep . map pretty

instance SMTPretty [SSymbol] where
  spretty = hsep . map pretty

instance SMTPretty [SExpr] where
  spretty = hsep . map pretty

instance SMTPretty [SAttribute] where
  spretty = hsep . map pretty

instance SMTPretty [SNumeral] where
  spretty = hsep . map pretty

-- Pretty instances
instance Pretty SExpression where
  pretty (SE expr) = parens $ pretty expr
  
instance Pretty SCmd where
  pretty (SetLogic logic)               = text "set-logic"   <+> pretty logic
  pretty (DeclFun  sym exprs expr)      = text "declare-fun" <+> pretty sym <+> parens (spretty exprs) <+> pretty expr
  pretty (DefFun   sym exprs sort expr) = text "define-fun"  <+> pretty sym <+> parens (spretty exprs) <+> pretty sort <+> pretty expr
  pretty (DeclSort sym num)             = text "declare-sort"<+> pretty sym <+> pretty num
  pretty (DefSort  sym syms expr)       = text "define-sort" <+> pretty sym <+> parens (spretty syms)  <+> pretty expr
  pretty (Assert   expr)                = text "assert"      <+> pretty expr
  pretty GetAsserts                     = text "get-assertions"
  pretty CheckSat                       = text "check-sat"
  pretty GetProof                       = text "get-proof"
  pretty GetUnsatCore                   = text "get-unsat-core"
  pretty (GetValue exprs)               = text "get-value" <+> parens (spretty exprs)
  pretty GetAssign                      = text "get-assigment"
  pretty (Push num)                     = text "push" <+> pretty num
  pretty (Pop  num)                     = text "pop"  <+> pretty num
  pretty (GetOpt keywrd)                = text "get-option" <+> pretty keywrd
  pretty (SetOpt keywrd attrval)        = text "set-option" <+> pretty keywrd <+> pretty attrval
  pretty (GetInfo keywrd)               = text "get-info"   <+> pretty keywrd
  pretty (SetInfo keywrd attrval)       = text "set-info"   <+> pretty keywrd <+> pretty attrval
  pretty Exit                           = text "exit"
  
instance Pretty SLogic where
  pretty logic = text $ show logic

instance Pretty SSymbol where
  pretty (SimpleSym sym) = pretty sym
  pretty (QuotedSym sym) = char '|' <> text sym <> char '|'
  pretty (ReservSym sym) = error "Reserved words are not true symbols." 

instance Pretty SSortExpr where
  pretty (SymSort sort)         = pretty sort
  pretty (FunSort sym sortexpr) = parens $ pretty sym <+> spretty sortexpr  

instance Pretty SExpr where
  pretty (LitExpr    lit)          = pretty lit
  pretty (IdentExpr  ident)        = pretty ident
  pretty (FnAppExpr  ident exprs)  = parens $ pretty ident <+> spretty exprs
  pretty (ForallExpr symsort expr) = parens $ text "forall" <+> parens (spretty symsort) <+> pretty expr 
  pretty (ExistsExpr symsort expr) = parens $ text "exists" <+> parens (spretty symsort) <+> pretty expr 
  pretty (LetExpr    symsort expr) = parens $ text "let" <+> parens (spretty symsort) <+> pretty expr 
  pretty (AttrExpr   expr attrs)   = parens $ char '!'   <+> pretty expr <+> spretty attrs -- TODO

instance Pretty SLiteral where
  pretty (NumLit num) = pretty num
  pretty (DecLit dec) = pretty dec
  pretty (BinLit bin) = text "0b" <> pretty bin
  pretty (HexLit hex) = text "0x" <> pretty hex
  pretty (StrLit str) = dquotes $ text str

instance Pretty SIdent where
  pretty (SymIdent sym)        = pretty sym
  pretty (IdxIdent sym nums)   = parens $ char '_'  <+> pretty sym   <+> spretty nums
  pretty (QlfIdent ident sort) = parens $ text "as" <+> pretty ident <+> pretty sort
    
instance Pretty SAttribute where
  pretty (AttrKey      keywrd)     = pretty keywrd
  pretty (AttrKeyValue keywrd val) = pretty keywrd <+> pretty val

-- At the moment SAttrValue is a string
--instance Pretty SAttrValue where
--  pretty attrval = undefined

instance Pretty SKeyword where
  pretty (Keyword s) = char ':' <> pretty s
