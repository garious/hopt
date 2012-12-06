-- Oddly, GHC 7.6 finds Parsec 3.1.3 Safe, but GHC 7.4 does not.  So we mark
-- this module Trustworthy until Travis-CI upgrades.
{-# LANGUAGE Trustworthy #-}

-- | The command-line argument parser for hopt

module HoptArgParser where

import Control.Lens.Setter
  ( (%=)   -- modify a lens from a monad
  , (.=)   -- assign a lens from a monad
  )

-- From mtl package
import Control.Monad.State
  ( runState
  )

import Control.Applicative
  ( (<*)
  , (*>)
  , (<$>)
  , (<*>)
  , (<|>)
  )
import Text.Parsec
  ( string
  , (<?>)
  , skipMany
  , satisfy
  , eof
  , many
  , many1
  , spaces
  , runParserT
  , try
  , unexpected
  , ParseError
  )
import HoptArgData

-- | By default, do no optimizations on stdin, compile to bitcode and output to stdout
defaultCfg :: Cfg
defaultCfg = Cfg [] "-" "-" False

-- | Parse the argument list.  Return either an error of the Hopt configuration
parseArguments :: [String] -> String -> Either ParseError Cfg
parseArguments optPassNames str = const cfg `fmap` result
  where
    (result, cfg) = runState (argParser str) defaultCfg

    argParser s = runParserT arguments () "argument list" s

    arguments = skipMany argument <* eof

    argument = optionArg
           <|> optPassFlag optPassNames
           <|> inputFileArg
           <?> "argument"

    optionArg = textOutputFlag
            <|> outputFileFlag
            <?> "option"

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
      <?> "input file"
    
    optPassFlag nms = do
        p <- string "-" *> flagArg
        if elem p nms
          then optPasses %= (++ [p])
          else unexpected $ "'" ++ p ++ "'.  Expected the name of a known optimization pass."
      <?> "name of optimization pass"
    
    flag s = try (string "-" *> string s) *> spaces <?> "flag"
    
    argArg = (:) <$> satisfy (/= '-') <*> many (satisfy (/= ' ')) <* spaces <?> "optimization flag"
    
    flagArg = many1 (satisfy (/= ' ')) <* spaces <?> "flag argument"
