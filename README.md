# Simplify-bkg
Program used for simplifying CFG grammar by applying Alg 4.3

Usage: simplify-bkg (-i | -1 | -2) [FILENAME]
Program reads CFG grammar from file given as second argument or standart input in case filepath is not specified. Depending on the first argument given output is:
* i parsed grammar
* 1 grammar consisting only from necessary nonterminals (Alg 4.1)
* 2 grammar consisting only from reachable symbols (Alg4.2)

For ease of use makefile is provided with following rules to run program:
make all - compiles program
make run1 - runs program with -i parameter on sample input
make run2 - runs program with -1 parameter on sample input
make run3 - runs program with -2 parameter on sample input
make doc - creates documentation in doc folder created by haddock

Algorithms referenced: https://wis.fit.vutbr.cz/FIT/st/course-files-st.php?file=%2Fcourse%2FTIN-IT%2Ftexts%2Fopora.pdf

## Dependancies
* Program uses optparse-applicative for parsing program arguments
* Program uses ReadP parser for parsing program input
* Program was developed and tested on haskell-platform package from Ubuntu repositories using ghci version 7.10.3

## Credits
* Method of parsing program input and using data types https://github.com/Tr1p0d/universal-turing-machine
* Method of parsing program arguments https://hackage.haskell.org/package/optparse-applicative
