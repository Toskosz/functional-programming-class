1. dependencias
    cabal install alex happy BNFC
2. pre requisitos
    bnfc LF1.cf
    alex LexLF.x
    happy ParLF.y
3. compilacao
    ghc Interpret.hs
4. Rodar os programas
    ./Interpret < examples/ex1.lf1
    ./Interpret < examples/ex2.lf1
    ./Interpret < examples/ex4.lf1
