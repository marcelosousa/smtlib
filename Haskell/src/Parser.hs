module Parser where
  
import Text.ParserCombinators.UU.Utils (runParser)

import Parser.Base
import Parser.Core

-- Run Parsers
parse f = do s <- readFile f
             print $ runParser "Error" pSMod s

--parse = runParser "Error"


