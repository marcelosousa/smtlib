module Test where

import Language.SMTLib2
import Language.SMTLib2.Parser
import Language.SMTLib2.Printer
import Test.HUnit
import System.Directory
import System.FilePath

getTestDir :: IO FilePath
getTestDir = do pwd <- getCurrentDirectory
                return $ takeDirectory pwd </> "tests"

getTests :: IO [FilePath]
getTests = do td <- getTestDir
              fps <- getDirectoryContents td
              return $ filter (\f -> takeExtensions f == ".smt2") fps
             
runTests = do td <- getTestDir
              lfp <- getTests
              let tests = TestList $ map (\f -> genTest (td </> f)) lfp
              runTestTT tests
              
genTest :: FilePath -> Test
genTest p = TestLabel p $ TestCase $ do s <- readFile p
                                        assertEqual "diffs found" s (show $ prettyprint $ parse s)


