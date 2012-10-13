{-# LANGUAGE Safe #-}

module Main where

import System.Environment
  ( getArgs
  )
import ArgParser
  ( parseArguments
  )
import Opt
  ( compile
  , optPassNames
  )

main :: IO ()
main = do
    xs <- getArgs 
    compile $ parseArguments optPassNames (unwords xs)
