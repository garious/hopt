{-# LANGUAGE Safe #-}

module Opt where

import Prelude hiding (null)
import ArgParser
  ( Cfg(Cfg)
  , optPasses
  , inFile
  , outFile
  , optPasses
  , _isText
  )
import System.IO
  ( openFile
  , IOMode(WriteMode)
  , stdout
  )
import Data.Monoid
  ( mempty
  , mappend
  , Monoid
  )

-- From fclabels package
import Data.Label.Pure
  ( get
  )

-- From IterIO package
import Data.IterIO
  ( Iter
  , Inum
  , ChunkData
  , chunkShow
  , (|$)
  , (|.)
  , (.|)
  , enumFile'
  , enumStdin
  , mkInum
  , handleI
  , lineI
  , inumNop
  , dataI
  )
import Data.IterIO.Iter
  ( null
  )
--import Data.IterIO.Parse
--  ( (<|>)
--  , string
--  , (<:>)
--  , (<*)
--  , (*>)
--  , skipMany
--  , satisfy
--  , eofI
--  , eord
--  , whileI
--  , while1I
--  , skipWhileI
--  )
import Data.ListLike
  ( ListLike
  , singleton
  )

import qualified Data.ByteString.Lazy.Char8 as L

type OptPass = Iter Flow IO Flow

compile :: Cfg -> IO ()
compile Cfg{_isText=False} = error "Write bitcode?  What am I, a compiler?  Please come back with -S."
compile cfg  = do
    hdl <- outPath == "-" ? (return stdout, openFile outPath WriteMode)
    input |$ parseFlow .| optimize (get optPasses cfg) .| printFlow .| handleI hdl
  where
    input = inPath == "-" ? (enumStdin, enumFile' inPath)
    inPath  = get inFile cfg
    outPath = get outFile cfg

data Flow = Nop
          | BasicBlock [Statement]
  deriving (Show, Eq)

data Statement = Declaration String Type
               | Assignment String (Expression Int)
               | Garbage String
  deriving (Show, Eq)

data Type = TyInt
          | TyAmbiguous [Type]
          | TyUndiscovered
  deriving (Show, Eq)

data Expression a = Constant a
  deriving (Show, Eq)

instance Monoid Flow where
    mempty                                = Nop
    mappend Nop x                         = x
    mappend x Nop                         = x
    mappend (BasicBlock x) (BasicBlock y) = BasicBlock $ x ++ y

instance ChunkData Flow where
    null Nop     = True
    null _       = False
    chunkShow    = show

parseFlow :: Inum L.ByteString Flow IO a
parseFlow = mkInum parseFlow'

parseFlow' :: Iter L.ByteString IO Flow
parseFlow' = (lineI >>= return . BasicBlock . singleton . Garbage . L.unpack)
         -- <|> (lineI >>= return . BasicBlock . singleton . Garbage . L.unpack)

optimize :: [String] -> Inum Flow Flow IO a
optimize []     = inumNop
optimize (x:xs) = maybe (error x) mkInum (lookup x optPassMap) |. optimize xs

printFlow :: Inum Flow L.ByteString IO a
printFlow = mkInum prettyFlow

prettyFlow :: Iter Flow IO L.ByteString
prettyFlow  = dataI >>= return . L.pack . (++"\n") . show

-- | An alternative to the if-then-else syntax
(?) :: Bool -> (a, a) -> a
b ? (t, e) = if b then t else e
infixl 1 ?

optPassNames :: [String]
optPassNames = map fst optPassMap

-- | A map from command-line names to the function that implements an
--   optimization pass
optPassMap :: [(String, OptPass)]
optPassMap = [
    ("constprop", constProp)
  ]

-- \ The Constant Propogation pass
constProp :: OptPass
constProp = dataI

