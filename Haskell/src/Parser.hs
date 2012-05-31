module Parser where
  
import Text.ParserCombinators.UU.Utils (runParser)

import Parser.Core

-- Run Parsers
parse = runParser "Error" pSMod


