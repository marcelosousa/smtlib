module Main where

import Language.SMTLib2
import Language.SMTLib2.Parser
import Language.SMTLib2.Printer
import Language.SMTLib2.Builder

main :: IO ()
main = putStrLn "SMT-Lib v2"

pp :: FilePath -> IO ()
pp f = do s <- readFile f
          print $ prettyprint $ parse s

