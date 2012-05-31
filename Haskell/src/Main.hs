module Main where

import SMTLib2
import Parser
import Printer

main :: IO ()
main = putStrLn "SMT-Lib v2"

pp :: FilePath -> IO ()
pp f = do s <- readFile f
          print $ prettyprint $ parse s

