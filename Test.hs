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

main :: IO ()
main = do
    bs <- sequence tests
    if and bs
      then do
        putStrLn "passed!"
      else
        exitFailure

tests :: [IO Bool]
tests = [
    runHopt "ConstPropTestTrivial"                  ".ll" ["-constprop"]
  , runHopt "ConstPropTestBranch"                   ".ll" ["-constprop"]
  , runHopt "DeadInstructionEliminationTestTrivial" ".ll" ["-die"]
  ]

runHopt :: String -> String -> [String] -> IO Bool
runHopt nm ext optPasses = do
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

hoptPath :: FilePath
hoptPath = "dist/build/hopt/hopt"
