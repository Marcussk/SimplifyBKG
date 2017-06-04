-- simplify-bkg
-- Marek Beňo
-- xbenom01

{-|
Module      : ArgumentParser
Description : Parser module for program arguments

Module which uses optparse-aplicative for parsing program arguments
-}

module ArgumentParser where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad

import Types

-- | parses program arguments into @Arguments@ structure
argumentParser :: Parser Arguments
argumentParser = Arguments
  <$> (dumpParser <|> alg1Parser <|> alg2Parser)
  <*> argument str (metavar "FILENAME" <> help "Filename of input file" <> value "")

-- | parses @Dump@ action in @Arguments@ structure
dumpParser :: Parser ArgumentAction
dumpParser = flag' Dump
  ( short 'i'
  <> help "specifies action as printing parsed input"
  )

-- | parses @Alg1@ action in @Arguments@ structure
alg1Parser :: Parser ArgumentAction
alg1Parser = flag' Alg1
  ( short '1'
  <> help "specifies action as aplying algorithm 4.1 to parsed grammar"
  )

-- | parses @Alg2@ action in @Arguments@ structure
alg2Parser :: Parser ArgumentAction
alg2Parser = flag' Alg2
  ( short '2'
  <> help "specifies action as aplying algorithm 4.1 to parsed grammar"
  )

-- | provides program info
opts = info (argumentParser <**> helper)
          ( fullDesc
         <> progDesc "parses CFG grammar from input stream and simplifies it."
         <> header "Program for simplyfing CFG grammar" )
