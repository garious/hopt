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

main :: IO ()
main = do
    setCurrentDirectory "Transforms"
    bs <- sequence tests
    if and bs
      then do
        putStrLn "passed!"
      else
        exitFailure

tests :: [IO Bool]
tests = [
    runHopt "ConstPropTest/Basic"                  ".ll" ["-constprop"]
  , runHopt "ConstPropTest/Branch"                 ".ll" ["-constprop"]
  , runHopt "CopyPropTest/Basic"                   ".ll" ["-copyprop"]
  , runHopt "DeadInstructionEliminationTest/Basic" ".ll" ["-die"]
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
hoptPath = "../dist/build/hopt/hopt"
