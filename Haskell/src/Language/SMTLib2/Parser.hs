module Language.SMTLib2.Parser where
  
import Text.ParserCombinators.UU.Utils (runParser)

import Language.SMTLib2.Parser.Core

-- Run Parsers
parse = runParser "Error" pSMod


