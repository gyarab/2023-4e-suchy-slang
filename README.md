<h3 align="center">Programovací jazyk Slang</h3>

> Slang je víceúčelový programovací jazyk založený na principu datových proudů.
> V této práci jsem definoval jeho syntaxi a naprogramoval překladač do LLVM IR.

### Vyzkoušení

`slangc` je dostupný na SVS.

### Kompilace

1. Nainstalujte Haskell Stack - https://docs.haskellstack.org/en/stable/ (`curl -sSL https://get.haskellstack.org/ | sh`)
2. Naklonujte repozitář
3. Spusťte `stack build`
4. Zkopírujte zkompilovaný překladač do pracovního adresáře (`cp $(stack path --local-install-root)/bin/slangc .`)
5. Spusťte pomocí `./slangc`

### Použití

 - Závislosti: llvm překladač - llc
 - Závislosti pro linkování: gcc - zajištujě správnou konfiguraci `ld` a soubory `crt{1,i,n}.o`

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

Je možné, že program nebude možné nalinkovat. V tom případě vytvbořte soubor `.o` pomocí spínače `-L` a
spusťte linker sami. Například takto:
```bash
ld\
 -dynamic-linker /lib64/ld-linux-x86-64.so.2\
 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o\
 <soubor.o>\
 /usr/lib/x86_64-linux-gnu/crtn.o\
 -lc
```

Pomocí `slangc` můžete zkompilovat například ukázky v `examples/`, nebo `poster.slg`.
