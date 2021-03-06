{-# LANGUAGE Safe #-}

-- | The command-line argument parser for hopt

module HoptArgParser where

import Control.Lens.Setter
  ( (%=)   -- modify a lens from a monad
  , (.=)   -- assign a lens from a monad
  )

-- From mtl package
import Control.Monad.Trans.State
  ( runState
  )

import Control.Applicative
  ( (<*)
  , (*>)
  , (<$>)
  , (<*>)
  , (<|>)
  )
import Text.Parsec.Prim
  ( (<?>)
  , many
  , try
  , runParserT
  , unexpected
  , skipMany
  )
import Text.Parsec.String
  ( -- Import instances
  )
import Text.Parsec.Combinator
  ( eof
  , many1
  )
import Text.Parsec.Char
  ( string
  , satisfy
  , spaces
  )
import Text.Parsec.Error
  ( ParseError
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

