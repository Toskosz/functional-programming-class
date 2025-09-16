1. Install dependencies: If not already installed, you need the alex and happy tools. You can install them using cabal:
    cabal install alex happy BNFC
2. Generate parser and lexer: Run the following commands to generate the Haskell files for the lexer and parser:
    bnfc LF1.cf
    alex LexLF.x
    happy ParLF.y
3. Compile the project: Compile the main interpreter file using ghc:
    ghc Interpret.hs
4. Run the interpreter: You can then run the compiled interpreter with any of the example files by piping the file content to the standard input of the program:
    ./Interpret < examples/ex1.lf1
    ./Interpret < examples/ex2.lf1
    ./Interpret < examples/ex4.lf1
