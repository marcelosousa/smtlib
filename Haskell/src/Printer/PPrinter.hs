module Printer.PPrinter where

import SMTLib2.Base

import UU.PPrint

instance Pretty SExpression where
  pretty (SE expr) = parens $ pretty expr
  
instance Pretty SCmd where
  pretty (SetLogic logic)               = text "set-logic"   <+> pretty logic
  pretty (DeclFun  sym exprs expr)      = text "declare-fun" <+> pretty sym <+> parens (pretty exprs) <+> pretty expr
  pretty (DefFun   sym exprs sort expr) = text "define-fun"  <+> pretty sym <+> parens (pretty exprs) <+> pretty sort <+> pretty expr
  pretty (DeclSort sym num)             = text "declare-sort"<+> pretty sym <+> pretty snum
  pretty (DefSort  sym syms expr)       = text "define-sort" <+> pretty sym <+> parens (pretty syms)  <+> pretty expr
  pretty (Assert   sexpr)               = text "assert"      <+> pretty expr
  pretty GetAsserts                     = text "get-assertions"
  pretty CheckSat                       = text "check-sat"
  pretty GetProof                       = text "get-proof"
  pretty GetUnsatCore                   = text "get-unsat-core"
  pretty (GetValue exprs)               = text "get-value" <+> pretty exprs
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
  pretty sym = undefined

instance Pretty SSortExpr where
  pretty sortexpr = undefined

instance Pretty SExpr where
  pretty expr = undefined

instance Pretty SAttrValue where
  pretty attrval = undefined
{-  
  data SCmd = SetLogic     SLogic
            | DeclFun      SSymbol [SSortExpr] SSortExpr
            | DefFun       SSymbol [(SSymbol,SSortExpr)] SSortExpr SExpr
            | DeclSort     SSymbol SNumeral
            | DefSort      SSymbol [SSymbol] SExpr
            | Assert       SExpr
            | GetAsserts
            | CheckSat
            | GetProof
            | GetUnsatCore
            | GetValue     [SExpr]
            | GetAssign
            | Push         SNumeral
            | Pop          SNumeral
            | GetOpt       SKeyword
            | SetOpt       SKeyword SAttrValue
            | GetInfo      SKeyword
            | SetInfo      SKeyword SAttrValue
            | Exit
      deriving Show
-}