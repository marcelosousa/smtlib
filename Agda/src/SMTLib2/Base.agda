module SMTLib2.Base where

open import Data.List
open import Data.List.NonEmpty
open import Data.Product
open import Data.Nat
open import Data.String

data Numeral : Set where
  numeral : ℕ -> Numeral

data Decimal : Set where

data Binary : Set where

data Hex : Set where

data Str : Set where
  string : String -> Str

data Keyword : Set where

data Attr-Value : Set where

data Symbol : Set where

data SortExpr : Set where
