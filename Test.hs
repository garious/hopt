module Main where

import System.Exit
  ( exitFailure
  )
import System.Process
  ( readProcess
  )
import System.IO
  ( stderr
  , hPutStrLn
  )
import System.Directory
  ( setCurrentDirectory
  )

-- | Entry point for the test suite
main :: IO ()
main = do
    setCurrentDirectory "Transforms"
    bs <- sequence tests
    if and bs
      then do
        putStrLn "passed!"
      else
        exitFailure

-- | List of tests to execute
tests :: [IO Bool]
tests = [
    run "ConstPropTest/Basic"                       ".ll" ["-constprop"]
  , run "ConstPropTest/BasicFlush"                  ".ll" ["-constprop"]
  , run "ConstPropTest/Branch"                      ".ll" ["-constprop"]
  , run "CopyPropTest/Basic"                        ".ll" ["-copyprop"]
  , run "CopyPropTest/BasicFlush"                   ".ll" ["-copyprop"]
  , run "DeadInstructionEliminationTest/Basic"      ".ll" ["-die"]
  , run "DeadInstructionEliminationTest/BasicFlush" ".ll" ["-die"]
  ]

-- | Run a test
run :: String -> String -> [String] -> IO Bool
run nm ext optPasses = do
    act <- readProcess hoptPath ("-S" : optPasses ++ [nm ++ ext]) ""
    gld <- readFile (nm ++ ".gold" ++ ext)
    if act /= gld
      then do
        hPutStrLn stderr $ "FAIL! " ++ nm
        hPutStrLn stderr "expected:"
        hPutStrLn stderr gld
        hPutStrLn stderr " but got:"
        hPutStrLn stderr act
        return False
      else
        return True

-- | Path to hopt
hoptPath :: FilePath
hoptPath = "../dist/build/hopt/hopt"
