<h3 align="center">Programovací jazyk Slang</h3>

> Slang je víceúčelový programovací jazyk založený na principu datových proudů.
> V této práci jsem definoval jeho syntaxi a naprogramoval překladač do LLVM IR.

# Kompilace

1. Nainstalujte Haskell Stack - https://docs.haskellstack.org/en/stable/ (`curl -sSL https://get.haskellstack.org/ | sh`)
2. Naklonujte repozitář
3. Spusťte `stack build`
4. Zkopírujte zkompilovaný překladač do pracovního adresáře (`cp $(stack path --local-install-root)/bin/slangc .`)
5. Spusťte pomocí `./slangc`

# Použití
```
$ ./slangc
Usage: slangc [OPTION]... [-o OUTPUT_FILE] SOURCE_FILE...
  -h, --help       Show this help message
  --version        Show version info
  -OX              Optimization level (O0,O1,O2,O3) (default: O3)
  -o               Output file (default: a.out)
  -L, --no-link    Do not link the output object into an executable
  -v               Be verbose
  -S               Print LLVM code instead of compiling it
```

Pomocí `slangc` můžete zkompilovat například ukázky v `examples/`, nebo `poster.slg`.
