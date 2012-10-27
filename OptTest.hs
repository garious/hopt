{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

import Opt(parseAndPrint)
import Control.Exception(assert)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO

main :: IO ()
main = do
    test "%a = 4\n %b = %a\n ret %b" "ret 4\n"

test :: L.ByteString -> L.ByteString -> IO ()
test act ex = do
    txt <- enumPure act |. parseAndPrint ["constprop"] |$ pureI
    assert (txt == ex) noOp

noOp :: Monad m => m ()
noOp = return ()

