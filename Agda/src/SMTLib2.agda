module SMTLib2 where

open import Data.List
open import Data.List.NonEmpty
open import Data.Product
open import Data.Nat
open import Data.String
open import SMTLib2.Base
  
data Logic : Set where
  AUFLIA : Logic

data Id : Set where

data Expr : Set where
  numeral : Numeral -> Expr
  decimal : Decimal -> Expr
  string  : Str  -> Expr
  binary  : Binary -> Expr
  hex     : Hex -> Expr
  ident   : Id -> Expr
  _↠_     : Id -> List⁺ Expr -> Expr   
    

data Command : Set where
  set-logic    : Logic  -> Command
  declare-fun  : Symbol -> List SortExpr -> SortExpr -> Command
  define-fun   : Symbol -> List (Symbol × SortExpr) -> SortExpr -> Expr -> Command 
  declare-sort : Symbol -> Numeral -> Command
  define-sort  : Symbol -> List⁺ Symbol -> Expr -> Command
  assert       : Expr   -> Command
  get-assertions : Command
  check-sat    : Command
  get-proof    : Command
  get-unsat-core : Command
  get-value : List⁺ Expr -> Command
  get-assignment : Command
  push : Numeral -> Command
  pop : Numeral -> Command
  get-option : Keyword -> Command
  set-option : Keyword -> Attr-Value -> Command
  get-info : Keyword -> Command
  set-info : Keyword -> Attr-Value -> Command
  exit : Command

data S-Expression : Set where
  S-Expr  : Command -> S-Expression
  Comment : S-Expression

data Module : Set where
  Mod : List S-Expression -> Module


