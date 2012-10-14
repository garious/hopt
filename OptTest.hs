{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

import Opt(parseAndPrint)
import Control.Exception(assert)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO

main :: IO ()
main = do
    test "var a = 4; var b = a; return b" "return 4\n"

test :: L.ByteString -> L.ByteString -> IO ()
test act ex = do
    txt <- enumPure act |. parseAndPrint ["constprop", "unassigned"] |$ pureI
    assert (txt == ex) $ return ()

