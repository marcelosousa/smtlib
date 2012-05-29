module SMTLib2.Base where

import Data.Sequence

type SMod = [SExpression] -- One or more SExpr

type SExpression = SCmd

-- data SExpression = Token    SToken
--                  | SeqSExpr [SExpression] 
--     deriving Show
-- 
-- data SToken = LitToken SLiteral
--             | ResToken SReserved
--             | SymToken SSymbol
--             | KeyToken SKeyword
--     deriving Show
    
-- The semantics of literals changes
-- with the Logic.
data SLiteral = NumLit SNumeral 
              | DecLit SDecimal
              | BinLit SBinary
              | HexLit SHex
              | StrLit SString
    deriving Show

-- data SReserved = ResWrd SResWrd
--                | Cmd    SCmd
--     deriving Show
--     
-- data SResWrd = RWpar
--              | RWNUMERAL
--              | RWDECIMAL
--              | RWSTRING
--              | RW_
--              | RWExclMark
--              | RWas
--              | RWlet
--              | RWforall
--              | RWexists
--     deriving Show         

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

data SLogic = AUFLIA | AUFLIRA | AUFNIRA | LRA | QF_ABV 
            | QF_AUFBV | QF_AUFLIA | QF_AX | QF_BV | QF_IDL 
            | QF_LIA | QF_LRA | QF_NIA | QF_NRA | QF_RDL 
            | QF_UF | QF_UFBV | QF_UFIDL | QF_UFLIA | QF_UFLRA
            | QF_UFNRA | UFLRA | UFNIA
    deriving (Show,Read)
    
-- Sort Expression <sort-expr>
data SSortExpr = SymSort SSort
               | FunSort SSymbol [SSortExpr]
    deriving Show
    
data SSort = Sort String
    deriving Show

-- Token Types
data TokenTy = NumeralTy
             | DecimalTy
             | BinaryTy
             | HexTy
             | StringTy
             | SSymbolTy
             | KeywordTy
             | ReservedTy

type SNumeral = Int
type SDecimal = Double

type SBinary = [BDigit]        
type BDigit = Char

type SHex = [HDigit]
type HDigit = Char

type SString = String

data SSymbol = SimpleSym SimpleSym
             | QuotedSym QuotedSym
    deriving Show
    
type SimpleSym = String
type QuotedSym = String

data SKeyword  = Keyword String
    deriving Show
    
-- 3.6 Attributes 
data SAttribute = AttrKey      SKeyword
                | AttrKeyValue SKeyword SAttrValue
    deriving Show
    
type SAttrValue = String -- SExpression

-- 3.7 Expressions (<expr>)
data SExpr = LitExpr    SLiteral
           | IdentExpr  SIdent  -- can also be a qualified-identifier
           | FnAppExpr  SIdent [SExpr]
           | ForallExpr [(SSymbol, SSort)] SExpr
           | ExistsExpr [(SSymbol, SSort)] SExpr
           | LetExpr    [(SSymbol, SSort)] SExpr
           | AttrExpr   SExpr [SAttribute]
    deriving Show

-- Identifiers (<identifier>) 
data SIdent = SymIdent SSymbol           -- <symbol>
            | IdxIdent SSymbol [SNumeral] -- ( _ <symbol> <numeral>+)
            | QlfIdent SIdent SSort       -- (qualified-identifier) (as ident sort)
    deriving Show