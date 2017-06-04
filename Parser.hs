-- simplify-bkg
-- Marek Beňo
-- xbenom01

{-|
Module      : Parser
Description : Parser module for BKG grammar

Module which handles parsing of input stream into @TGrammar@ using ReadP
-}

module Parser
  where

import Text.ParserCombinators.ReadP
import Types
import Debug.Trace
import Data.Char

-- | runs @grammarParser@ on given input @contents@
parseGrammar :: String -> Either TParserError TGrammar
parseGrammar contents = case readP_to_S grammarParser contents of
  [(a,_)] -> Right a
  _ -> Left TCorruptedGrammar

-- | readP parser for parsing BKG grammar
grammarParser :: ReadP TGrammar
grammarParser = do
  nonterminals <- parseNonterminals
  newLine
  terminals <- parseTerminals
  newLine
  startSymbol <- parseNonterminal
  newLine
  rules <- parseRules
  eof
  return $ TGrammar nonterminals terminals startSymbol rules

-- | represents single Char '\n'
newLine :: ReadP Char
newLine = char '\n'

-- | represents single Char ','
comma :: ReadP Char
comma = char ','

-- | represents single Char '-'
hyphen :: ReadP Char
hyphen = char '-'

-- | represents single Char '>'
gthan :: ReadP Char
gthan = char '>'

-- | represents predicate whether Char is Nonterminal
isNonterminal :: Char -> Bool
isNonterminal c  = (isUpper c) && ((c /= ',') && (c /= '\n') && (c /= '-'))

-- | readP parser for single Nonterminal
parseNonterminal :: ReadP Nonterminal
parseNonterminal = satisfy isNonterminal

-- | readP parser for Nonterminals separated by comma character
parseNonterminals :: ReadP [Nonterminal]
parseNonterminals = sepBy1 parseNonterminal comma

-- | represents predicate whether Char is Terminal
isTerminal :: Char -> Bool
isTerminal c = (isLower c) && ((c /= '\n') && (c /= ','))

-- | readP parser for single Terminal
parseTerminal :: ReadP Terminal
parseTerminal = satisfy isTerminal

-- | readP parser for Terminals separated by comma character
parseTerminals :: ReadP [Terminal]
parseTerminals = sepBy1 parseTerminal comma

-- | represents predicate whether char can occur on right side of rule
isRightSideTerm :: Char -> Bool
isRightSideTerm c = (((isLower c) || (isUpper c) || (c == '#')) && (c /= '\n'))

-- | readP parser for right side of a rule
parseRightSide :: ReadP String
parseRightSide = many1 $ satisfy isRightSideTerm

-- | readP parser for sequence of Rules, each on single line
parseRules :: ReadP [TRule]
parseRules = many1 $ do
    r <- parseRule
    newLine
    return r
  where
    parseRule = do
      left <- parseNonterminal
      hyphen
      gthan
      right <- parseRightSide
      return $ TRule left right

