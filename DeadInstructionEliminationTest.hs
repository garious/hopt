{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import LlvmParser
  ( parseFlow
  )
import DeadInstructionElimination
  ( deadInstructionElimination
  )
import LlvmPrinter
  ( printFlow
  )
import Control.Exception
  ( assert
  )
import Data.IterIO
  ( (|.)
  , (|$)
  , enumPure
  , pureI
  )
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    test "define i32 @a(){%a = 4\n %b = %a\n ret i32 %a\n}" "define i32 @a() {\n  %a = 4\n  ret i32 %a\n}\n"

test :: L.ByteString -> L.ByteString -> IO ()
test act ex = do
    txt <- enumPure act |. parseFlow |. deadInstructionElimination |. printFlow |$ pureI
    assert (txt == ex) noOp

noOp :: Monad m => m ()
noOp = return ()

