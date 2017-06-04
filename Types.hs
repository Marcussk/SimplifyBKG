-- simplify-bkg
-- Marek Beňo
-- xbenom01

{-|
Module      : Types
Description : User types for representing CFG grammar and program arguments
-}

module Types
  where

-- | type synonym for representing Nonterminal as Char [A-Z]
type Nonterminal = Char
-- | type synonym for representing Terminal as Char [a-z]
type Terminal = Char
-- | type synonym for error message during grammar sanity check
type ErrorMessage = String

-- | represents CFG Grammar parsed from input
data TGrammar = TGrammar
  { nonterminals :: [Nonterminal]
  , terminals :: [Terminal]
  , startSymbol :: Nonterminal
  , rules :: [TRule]
  } deriving (Show)

-- | represents single rule
data TRule = TRule
  { leftside :: Nonterminal -- ^ single Nonterminal in left side of rule
  , rightside :: String -- ^ list of Nonterminals, Terminals or epsilon
  } deriving (Show)

-- | errors encountered when parsing grammar
data TParserError
  = TUnknownError
  | TCorruptedGrammar
  deriving (Show)

-- | represents right side of rule
data RightsideExpr
  = Terminal
  | Nonterminal
  deriving (Show)

-- | represents action taken according to first argument
data ArgumentAction
  = Dump -- ^ represents -i option, dump parsed grammar
  | Alg1 -- ^ represents -1 option, apply Alg 4.1
  | Alg2 -- ^ represents -2 option apply Alg 4.2
  deriving (Show)

-- | stores program arguments (-i|-1|-2) [path]
data Arguments = Arguments
  { argAction :: ArgumentAction
  , filePath :: FilePath
  } deriving (Show)
