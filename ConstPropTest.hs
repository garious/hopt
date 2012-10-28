{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import LlvmParser
  ( parseFlow
  )
import ConstProp
  ( constProp
  )
import ToLlvm
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
    test "%a = 4\n %b = %a\n ret %b" "ret 4\n"

test :: L.ByteString -> L.ByteString -> IO ()
test act ex = do
    txt <- enumPure act |. parseFlow |. constProp |. printFlow |$ pureI
    assert (txt == ex) noOp

noOp :: Monad m => m ()
noOp = return ()

