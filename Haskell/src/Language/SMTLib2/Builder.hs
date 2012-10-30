module Language.SMTLib2.Builder where

import Language.SMTLib2.Base

setlogic = SE . SetLogic
checksat = SE $ CheckSat
exit     = SE $ Exit
assert   = SE . Assert
setoption s = SE $ SetOpt (Keyword s) $ IdentExpr $ SymIdent $ SimpleSym "true"
declfun sym sexpr = SE $ DeclFun sym [] sexpr

-- SExpr
letin sym sexpr0 sexpr = LetExpr [(sym,sexpr0)] sexpr
sort sym i = SE $ DeclSort (SimpleSym sym) i
defsorti i     = SE $ DefSort  (SimpleSym $ "I" ++ show i) [] (IdentExpr $ IdxIdent (SimpleSym "BitVec") [i])
defsort  s   t = SE $ DefSort  (SimpleSym s) [] (IdentExpr $ SymIdent $ SimpleSym t)

declsort s n = SE $ DeclSort (SimpleSym s) n

toSExpr :: SSortExpr -> SExpr
toSExpr (SymSort s)          = IdentExpr $ SymIdent $ SimpleSym s
toSExpr (FunSort sym lssort) = undefined
toSExpr (BitVector n)        = IdentExpr $ IdxIdent (SimpleSym "BitVec") [n]
toSExpr (ArraySort s1 s2)    = FnAppExpr (SymIdent $ SimpleSym "Array") $ map toSExpr [s1,s2]
toSExpr (PairSort  s1 s2)    = FnAppExpr (SymIdent $ SimpleSym "Pair")  $ map toSExpr [s1,s2]
toSExpr (PointerSort s1)     = FnAppExpr (SymIdent $ SimpleSym "Pointer")  $ map toSExpr [s1]
