-- simplify-bkg
-- Marek Beňo
-- xbenom01

{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Parses bkg grammar from input and simplifies it (Alg 4.3)
Maintainer  : Marek Beňo, xbenom01@stud.fit.vutbr.cz
Stability   : Unknown
-}

module Main where

import System.Environment
import System.IO
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Debug.Trace
import Options.Applicative

import Types
import GrammarParser
import ArgumentParser

-- | prints Grammar to stdout in given format
dumpGrammar :: Either ErrorMessage TGrammar -> IO ()
dumpGrammar g = do
  case g of
    Right grammar -> do
      putStrLn $ intersperse ',' $ nonterminals grammar
      putStrLn $ intersperse ',' $ terminals grammar
      putChar $ startSymbol grammar
      putChar '\n'
      mapM_ printRule $ rules grammar
    Left error -> print error


-- | prints rule to stdout in given format
printRule r = do
  putChar $ leftside r
  putStr "->"
  putStrLn $ rightside r

-- | gets necessary Nonterminals from input grammar @gram@
-- according to Alg 4.1
getNecessary :: [Nonterminal]  -- ^ Necessary nonterminals N_i-1 from Alg 4.1
             -> TGrammar       -- ^ Input grammar
             -> [Nonterminal]  -- ^ Necessary nonterminals N_i from Alg 4.1
getNecessary nonterms gram = necessary where
  -- necessary nonterminals in the i-th step
  nontermsi = nub [x | x <- nonterminals gram, y <- rules gram, x == leftside y,
    all (`elem` ((terminals gram) ++ nonterms ++ "#")) (rightside y)]
  necessary = case (nontermsi \\ nonterms) of
    []        -> nonterms
    otherwise -> getNecessary (nub (nontermsi ++ nonterms)) gram

-- | applies Algorithm 4.1 to input grammar @g@ to get output
-- grammar @grammar@ consisting only from necessary nonterminals
prepareg1 :: TGrammar -> TGrammar
prepareg1 g = grammar where
  -- initial N0 according to Alg 4.1
  nonterms0 = [x | x <- nonterminals g, y <- rules g, x == leftside y,
    all (`elem` (terminals g) ++ "#") (rightside y)]
  -- necessary nonterminals according to Alg 4.1
  necessaryNont = sort $ nub $ getNecessary nonterms0 g
  -- rules consisting only necessary nonterminals
  necessaryRules = [r | r <- rules g, n <- necessaryNont, n == leftside r,
    all (`elem` ((terminals g) ++ necessaryNont) ++ "#") (rightside r)]
  grammar = TGrammar necessaryNont (terminals g) (startSymbol g) necessaryRules

-- | gets reachable Symbols from input grammar @gram@
-- according to Alg 4.2
getReachable :: [Nonterminal]  -- ^ Reachable symbols V_i-1 from Alg 4.2
             -> TGrammar       -- ^ Input grammar
             -> [Nonterminal]  -- ^ Reachable symbols V_i from Alg 4.2
getReachable symbols gram = reachable where
  -- reachable symbols in the i-th step
  symbolsi = nub [p | s <- symbols, r <- rules gram, p <- rightside r,
    s == leftside r]
  reachable = case (symbolsi \\ symbols) of
    []        -> symbols
    otherwise -> getReachable (nub (symbolsi ++ symbols)) gram

-- | applies Algorithm 4.2 to input grammar @g@ to get output
-- grammar @grammar@ consisting only from reachable nonterminals
prepareg2 :: TGrammar -> TGrammar
prepareg2 g = grammar where
  -- nonterminals and terminals that are reachable according to alg 4.2
  reachableSymbols = nub $ getReachable [(startSymbol g)] g
  reachableNont = sort $ filter (`elem` (nonterminals g)) reachableSymbols
  reachableTerms = sort $ filter (`elem` (terminals g)) reachableSymbols
  -- rules consisting only from reachable symbols
  reachableRules = [x | x <- rules g, y <- reachableNont, y == leftside x,
    all (`elem` (reachableTerms ++ reachableNont ++ "#")) (rightside x)]
  grammar = TGrammar reachableNont reachableTerms (startSymbol g) reachableRules

-- | check whether grammar is properly formed
validateGrammar :: TGrammar -> Either ErrorMessage TGrammar
validateGrammar g = if null $ rules g
  then Left "Invalid grammar: empty language"
  else
    if not $ elem (startSymbol g) (nonterminals g)
    then Left "Invalid grammar: start symbol not in nonterminals "
    else

    if length (nub (nonterminals g)) < length (nonterminals g)
    then Left "Invalid grammar: multiple occurences of nonterminals"
    else

    if length (nub (terminals g)) < length (terminals g)
    then Left "Invalid grammar: multiple occurences of nonterminals"
    else

    if not $ null $ invalidSymbols
    then Left ("Invalid grammar: rules contain invalid symbols" ++
      show invalidSymbols)
    else Right g
    where
      invalidSymbols = [s | r <- rules g, s <- rightside r] \\
        [s | r <- rules g, s <- rightside r,
          all (`elem` ((terminals g) ++ (nonterminals g) ++ "#")) (rightside r)]


-- | gets resulting grammar after applying algorithm
-- according to program arguments
-- sanity check is applied before each algorithm
handleGrammar :: ArgumentAction -> TGrammar -> Either ErrorMessage TGrammar
handleGrammar a g = grammar where
  grammar = case (validateGrammar g) of --sanity check after parsing
    Right g -> case a of
      Dump -> Right g
      Alg1 -> validateGrammar $ prepareg1 g --sanity check after alg1
      Alg2 -> case validateGrammar $ prepareg1 g of
            Right gram -> validateGrammar $ prepareg2 gram --sanity check after alg2
            Left error -> Left error
    Left error -> Left error

-- | gets input from input file or stdin according to arguments and
-- parses grammar from input and passes it to handler function
-- dumps resulting grammar or error
handleArguments :: Arguments -> IO ()
handleArguments Arguments{..} = do
  g <- case null filePath of
        True -> parseGrammar <$> hGetContents stdin
        False -> parseGrammar <$> readFile filePath
  case g of
        Right grammar -> dumpGrammar $ handleGrammar argAction grammar
        Left parseGrammarError -> print parseGrammarError

-- | Main program entry point, parses program arguments and
-- handles them to handler function
main :: IO ()
main = do
  handleArguments =<< execParser opts

