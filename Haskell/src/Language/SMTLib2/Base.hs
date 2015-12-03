module Language.SMTLib2.Base where

import Data.Sequence

type SMod = [SExpression] -- One or more SExpr

type SExpressions = [SExpression]
data SExpression = SE SCmd
                 | Comment
    deriving (Show,Eq,Ord)

-- data SExpression = Token    SToken
--                  | SeqSExpr [SExpression] 
--     deriving (Show,Eq,Ord)
-- 
-- data SToken = LitToken SLiteral
--             | ResToken SReserved
--             | SymToken SSymbol
--             | KeyToken SKeyword
--     deriving (Show,Eq,Ord)
    
-- The semantics of literals changes
-- with the Logic.
data SLiteral = NumLit SNumeral 
              | DecLit SDecimal
              | BinLit SBinary
              | HexLit SHex
              | StrLit SString
    deriving (Show,Eq,Ord)

-- data SReserved = ResWrd SResWrd
--                | Cmd    SCmd
--     deriving (Show,Eq,Ord)
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
--     deriving (Show,Eq,Ord)         


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
    deriving (Show,Eq,Ord)

data SLogic = AUFLIA | AUFLIRA | AUFNIRA | LRA | QF_ABV 
            | QF_AUFBV | QF_AUFLIA | QF_AX | QF_BV | QF_IDL 
            | QF_LIA | QF_LRA | QF_NIA | QF_NRA | QF_RDL 
            | QF_UF | QF_UFBV | QF_UFIDL | QF_UFLIA | QF_UFLRA
            | QF_UFNRA | UFLRA | UFNIA | HORN
    deriving (Show,Read,Eq,Ord)
    
-- Sort Expression <sort-expr>
data SSortExpr = SymSort SSort
               | FunSort SSymbol [SSortExpr]
	       | BitVector Int
               | ArraySort SSortExpr SSortExpr
               | PairSort SSortExpr SSortExpr
               | PointerSort SSortExpr
    deriving (Show,Eq,Ord)
    
type SSort = String

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
             | ReservSym ReservSym -- ^ We add an extra constructor for reserved words
    deriving (Show,Eq,Ord)
    
type SimpleSym = String
type QuotedSym = String
type ReservSym = String

newtype SKeyword  = Keyword String
    deriving (Show,Eq,Ord)
    
-- 3.6 Attributes 
data SAttribute = AttrKey      SKeyword
                | AttrKeyValue SKeyword SAttrValue
    deriving (Show,Eq,Ord)
    
type SAttrValue = SExpr -- SExpression that is not a keyword

-- 3.7 Expressions (<expr>)
data SExpr = LitExpr    SLiteral
           | IdentExpr  SIdent  -- can also be a qualified-identifier
           | FnAppExpr  SIdent [SExpr]
           | ForallExpr [(SSymbol, SSort)] SExpr
           | ExistsExpr [(SSymbol, SSort)] SExpr
           | LetExpr    [(SSymbol, SExpr)] SExpr
           | AttrExpr   SExpr [SAttribute]
           | ExtractExpr [SExpr]
           | ZeroExtExpr SExpr Int
           | SignExtExpr SExpr Int
           | Undef
    deriving (Show,Eq,Ord)

-- Identifiers (<identifier>) 
data SIdent = SymIdent SSymbol           -- <symbol>
            | IdxIdent SSymbol [SNumeral] -- ( _ <symbol> <numeral>+)
            | QlfIdent SIdent SSort       -- (qualified-identifier) (as ident sort)
    deriving (Show,Eq,Ord)
