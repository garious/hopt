{-# LANGUAGE Trustworthy #-}  -- Can't have both Safe Haskell and Template Haskell
{-# LANGUAGE TemplateHaskell #-}

module ArgParser where

-- From lens package
import Control.Lens.TH
  ( makeLenses
  )
import Control.Lens.Setter
  ( (%=)   -- modify a lens from a monad
  , (.=)   -- assign a lens from a monad
  )

-- From mtl package
import Control.Monad.State
  ( mzero
  , execState
  )

import Control.Applicative
  ( (<*)
  , (*>)
  , (<$>)
  , (<*>)
  )
import Text.Parsec
  ( string
  , (<|>)
  , skipMany
  , satisfy
  , eof
  , many
  , many1
  , runParserT
  , try
  )

data Cfg = Cfg {
    _optPasses  :: [String]
  , _inFile     :: FilePath
  , _outFile    :: FilePath
  , _isText     :: Bool      -- Output as text, not bitcode
  } deriving (Show, Eq)

-- Use Template Haskell to generate getters and setters
-- for each field in Cfg
makeLenses ''Cfg

-- By default, do no optimizations on stdin, compile to bitcode and output to stdout
defaultCfg :: Cfg
defaultCfg = Cfg [] "-" "-" False

parseArguments :: [String] -> String -> Cfg
parseArguments optPassNames = flip execState defaultCfg . argParser
  where
    argParser s = runParserT arguments () "" s

    arguments = skipMany argument <* eof

    argument = try textOutputFlag
           <|> try outputFileFlag
           <|> try (optPassFlag optPassNames)
           <|> inputFileArg

    textOutputFlag = do
        flag "S"
        isText .= True
    
    outputFileFlag = do
        flag "o"
        s <- flagArg
        outFile .= s
    
    inputFileArg = do
        s <- argArg
        inFile .= s
    
    optPassFlag nms = do
        p <- string "-" *> flagArg
        if elem p nms
          then optPasses %= (++ [p])
          else mzero
    
    flag s = string "-" *> string s *> skipSpace
    
    skipSpace = skipMany (satisfy (\c -> c == ' ' || c == '\t'))
    
    argArg = (:) <$> satisfy (/= '-') <*> many (satisfy (/= ' ')) <* skipSpace
    
    flagArg = many1 (satisfy (/= ' ')) <* skipSpace

