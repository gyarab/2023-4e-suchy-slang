#set text(font: "New Computer Modern", size: 11pt, lang: "cs", region: "cz")
#include "titulnistrany.typ"
#set page(numbering: "1")
#counter(page).update(1)
#include "obsah.typ"
#pagebreak()

#show par: set block(spacing: 0.65em)
#set par(justify: true, first-line-indent: 1.5em)

#set raw(syntaxes: "slang-hi.yaml")
#show raw.where(block: true): it => block( // nastavení code bloku
  stroke: rgb("#DDD"),
  width: 95%,
  inset: 10pt,
  radius: 0.3em,
  above: 2em,
  below: 2em,
  align(left, it)
)

#set heading(numbering: "1.")
#show heading: set block(above: 1.4em, below: 1em)

#let kod(caption, it) = figure(it, caption: caption, supplement: "Kód", kind: "code")

= Úvod

V rámci této práce jsem vytvořil překladač pro vlastní víceúčelový
programovací jazyk, který jsem pojmenoval Slang. Základní myšlenka jazyka je, že
většina programů zpracovává datové proudy: různými způsoby s nimi manipuluje a
upravuje je. V případě, že dat je velké množství, si často musí programátor psát
vlastní funkce, aby nemusel všechna data držet v paměti najednou. Pro populární
programovací jazyky již samozřejmě existují knihovny, které práci s datovými
proudy ulehčují. Mě ale zajímalo, jak by mohl vypadat jazyk, který má tuto
funkcionalitu zabudovanou do jeho samotné syntaxe.

== Původní znění zadání

Slang bude turingovsky kompletní programovací jazyk založený na principu
datových proudů. Cílem jazyka je prozkoumat možnost programování aplikací jako
program, co vytváří, mění a zpracovává datové proudy. Toto může mít několik
výhod, hlavně v možnosti paralelizace jednotlivých komponent.

Součástí jazyka bude:

1. silný typový systém,
2. vestavěná podpora principu datových proudů,
3. možnost využití paradigmatu funkcionálního programování (funkce a datové
   proudy lze spojovat a pracovat s nimi jako s hodnotami).

Součástí projektu bude samotná specifikace jazyka a k tomu přiložený LLVM
frontend (překladač do LLVM Itermediate Representation).

== Použité technologie

Pro kód překladače jsem zvolil čistě funkcionální programovací jazyk Haskell.
@haskell-org Tuto volbu jsem udělal, protože celý překladač je v zásadě
matematická funkce, která operuje nad textem. Haskell dokáže velmi jednoduše
skládat funkce dohromady a vytvářet z nich komplikovanější struktury. Zárověň
pro Haskell existuje mnoho knihoven například pro parsování.
#footnote[
  Parsování: viz @lexparse
]
Ostatní programovací jazyky většinou závisí na nástrojích, který parser vygenerují.

Překladač Slangu také negeneruje strojový kód, ale je jen frontend pro
LLVM - generuje LLVM IR (Intermediate Representation). LLVM IR je jazyk velmi
blízký assembly, ale není specifický pro architekturu procesoru @llvm. Překladač
do LLVM IR tak nemusí brát v potaz různé architektury.

LLVM má poté svůj vlastní překladač, který dokáže IR optimalizovat a přeložit do
strojového kódu. Vygenerované LLVM IR může tudíž být pomalé s instrukcemi navíc,
protože konečný program je optimalizovaný LLVM překladačem.

Další LLVM frontendy jsou například `rustc` (překladač jazyka Rust), nebo
`clang` (překladač jazyků C a C++). Jako poslední krok při překladu Slangu se
tedy spouští LLVM překladač, který zároveň provede optimalizaci kódu.

= Definice jazyka

Základní stavební jednotkou Slangu jsou transformátory datových proudů (dále
označovány jako *streamy*). Definují se pomocí klíčového slova ```slg stream``` na
úrovni souboru a mají jeden vstupní a výstupní typ.

Celý kód ve streamu je nekonečná smyčka, která se dá přerušit posláním hodnoty do výstupu.
Pokud bude ale vyžádána další výstupní hodnota, pak se smyčka opět spustí tam,
kde naposledy skončila. Všechny deklarované proměnné budou pořád existovat.

#kod(
  "Definice streamu",
  ```slg
  stream proud i32 -> i32 { }
  ```
)

== Datové typy a proměnné

Ve Slangu jsou všechny objekty typované. Je tedy nutné znát jejich typ už při
jejich deklaraci a ten nelze měnit. Proměnné je ale možné deklarovat znovu jako
jiný typ, ale po deklaraci ztratí svou původní hodnotu a v paměti mají nové místo.

Proměnné jsou deklarovány pomocí klíčového slova ```slg let```. Při deklaraci je
možné upřesnit typ, ale není potřeba. Když není typ specifikován, tak se
odvozen od nastavené hodnoty. Pokud se proměnná pouze deklaruje, ale není rovnou
inicializována hodnotou, je nutné typ specifikovat.

Základní datové typy jsou různé reprezentace čísel se znaménkem, kterým LLVM
rozumí. Pro Slang to jsou:

#align(center)[ ```slg bool, char, i32, i64, float, (void, ...)```  ]

Typy ```slg void``` a ```slg ...``` slouží pouze ke kompatibilitě s
externími funkcemi napsaných v jiných programovacích jazycích.

Z typů je možné skládat uspořádané n-tice tím, že je dáme společně do závorek.
Interně se těmto typům říká `tuple`. Z n-tice je možné jednotlivé prvky vybrat
pomocí číselných atribut. Pokud má proměnná `x` typ ```slg (i32, bool)```, pak
```slg x.0``` bude mít typ ```slg i32```.

Číselné konstanty jsou vždy typu ```slg i64```. Ke každému typu také existuje
ukazatel, který ukazuje na místo v paměti, kde je uložený právě ten typ
(například ```slg &i64``` je ukazatel na ```slg i64```). 

#kod(
  "Deklarace proměnných",
  ```slg
  let b: bool = false;

  // Typ není potřeba specifikovat. Typ proměnné x bude i64.
  let x = 0;

  let y: &i32; // Proměnná nemá hodnotu - není inicializována

  let t = (true, '\b');
  let prvni: bool = t.0;
  let druhy: &char = &t.1;
  ```
)

== Kolony

Důležitou funkcí Slangu jsou kolony streamů. Každá kolona se skládá z
jedné vstupní hodnoty a několika streamů. Speciální kolony ```slg in``` a
```slg out``` značí vstup a výstup aktuálního streamu.

Do _námi_ vytvořených kolon je možné poslat *pouze jednu* vstupní hodnotu hned
při jejich vytvoření, ale do kolony ```slg out``` můžeme posílat hodnoty vždy.
Hodnota poslaná do ```slg out``` pak bude dostupná v následujícím streamu v
koloně.

Z kolon můžeme hodnoty také vybírat (chytat) pomocí klíčového slova ```slg catch```.
Jestliže použijeme ```slg catch``` na kolonu, která nemá další výstup (je
ukončená), pak se aktuální stream a s ním i celá aktuální kolona ukončí.

Vytvořené kolony lze uložit do proměnné, ale tu nelze přepsat, ani použít jinak,
než při chytání výstupu kolony (viz @kolony).

Vytvoření kolony, která zčtyřnásobí svůj vstup:

#kod(
  "Kolona pro čtyřnásobek",
  ```slg
  stream double i64 -> i64 {
    (catch in) * 2 | out;
  }

  stream ukazka () -> () {
    let kolona = 2 | double | double;
    let ctyri: i64 = catch kolona; // 4
  }
  ```
) <quadruple>


== Operátory

Slang rozumí následujícím operátorům:

1. Matematické operátory: ```slg *, /, +, -```. Definované pro všechny číselné typy.
2. Porovnávací operátory: ```slg ==, !=, <, >, <=, >=```. Definované pro všechny číselné typy.
3. Logické operátory: ```slg !, &&, ||```. Definované pro typ ```slg bool```.
4. Operátory reference a dereference: ```slg &, *```. Definované pro hodnoty v
   proměnných a ukazatele.
6. Indexování: ```slg [idx]```. Definované pro ukazatele.
6. Operátory kolon: ```slg catch, |```
7. Přetypovací operátor: ```slg as```.\
   Přetypovat lze mezi sebou pouze celočíselné typy a ukazatele.

Priorita operací (sestupně):
#align(center, ```slg [idx], (&, *), as, (*, /), (+, -), (==, !=, <, >, <=, >=), (!, &&, ||), |, (catch, =)```)

#kod(
  "Operátory",
  ```slg
  let str: &char = "Hello World!";
  let prvniZnak = *str; // 'H'
  let druhyZnak = *str[1] as i32; // číselná hodnota 'e' s typem i32
  ```
)

== Vstupní bod

Vstupním bodem programu je stream `main`, který dostane známou dvojici
```slg (i32, &&char)``` reprezentující počet argumentů a ukazatel do pole
řetězců s argumenty. Zároveň musí mít návratový typ ```slg i32``` a vracet
standardní chybný kód.

#kod(
  "Vstupní bod programu",
  ```slg
  // vstupní bod programu
  stream main (i32, &&char) -> i32 {
    0 as i32 | out;
  }
  ```
)

#pagebreak()

== Řídící struktury

Slang podporuje dvě základní řídící struktury: ```slg if``` a ```slg while```.

#kod(
  "Řídící struktury if a while",
  ```slg
  let sum = 0;
  let i = 0;
  while sum < 100 {
    sum = sum + (i = i + 1);
  }
  if (i > 5) {
    "vetsi nez 5, mozna i 6, nebo 7, co ja vim" | out;
    if i > 6 {
      "vetsi nez 6" | out;
    } else if i > 7 {
      "vetsi nez 7" | out;
    }
  } else {
    "mensi nebo rovno 5" | out;
  }
  ```
)

== Volání externích funkcí

Pro případy, kdy je lepší napsat funkci v jiném jazyce, nebo když je potřeba
interagovat se systémem, je možné zavolat funkce z externích knihoven. Nejdříve
je ale nutné funkci deklarovat jako externí, aby Slang věděl, že existuje.
Například deklarace známé funkce `printf` vypadá takto:

#kod(
  "Deklarace externí funkce",
  ```slg
  extern fn printf (&char, ...) -> i32;
  ```
) <printfdecl>

V #ref(<printfdecl>, supplement: "kódu") můžeme vidět použití typu ```slg ...```,
který značí, že funkce má dynamický počet argumentů.

Před spuštěním programu je nutné zkompilovaný objekt spojit se statickou
knihovnou, nebo deklarovat dynamický linker, pomocí libovolného linkeru.
Přiložený skript `sc` používá LLVM linker `lld`.

= Překladač

Překladače jsou většinou rozděleny na několik částí, které na sebe
navazují. Postupně text zpracuje lexer, parser, statický analyzér a nakonec
se generuje výsledný kód. @CS143 V případě Slangu se jedná o LLVM IR.

Překladač Slangu má spojené fáze lexování a parsování a také analýzu s
generováním kódu.

== Lexer a parser <lexparse>

Překladače v první fázi provádí tokenizaci (lexikální analýzu) textu. Převádí
tím prostý text na syntakticky významné tokeny (lexémy). Zjednodušují tím práci
pro parser, který se nemusí tím pádem zabývat mezerami, nebo novými řádky, ale
může soubor chápat jako seznam klíčových slov, identifikátorů apod.

Pro Haskell existuje již několik knihoven, které buď přímo provádí lexikální
analýzu, nebo generují lexer. Například nástroj `alex` @alex dokáže z definice
tokenů jazyka ve svém vlastním formátu vygenerovat velmi rychlý lexer s
využitím deteriministických konečných automat. @alex-src Největší jejich výhodou
je složitost s jakou dokáží identifikovat lexémy v textu, která je lineární
($O(n)$) vzhledem k délce vstupních dat. @enwiki-automata @CS143-1

Parser následně z tokenů skládá syntaktický strom (AST), který reprezentuje
operace a jejich závislosti na jiných operacích (viz. @ast). Celá struktura
programu je v AST zaznamenána a například interpretované jazyky už mohou s tímto
přímo vykonat program.

#figure(
  image("images/ast.svg", width: 60%),
  caption: [Syntaktický strom while smyčky s deklarací proměnné],
) <ast>

Pro psaní parserů existuje také mnoho knihoven. Například `happy` je podobný
nástroj, jako `alex` (také od stejného vývojáře) a generuje optimalizované
parsery z gramatiky v Backus-Naurově normální formě. Happy umí vygenerovat LALR
a GLR parsery. @happy Oba typy parserů pochází z rodiny LR (#text(weight:
"bold", [L])eft to Right, #text(weight: "bold", [R])ightmost derivation)
parserů.

Slang ale nepoužívá `alex`, ani `happy`. `alex` byl dobrý kandidát pro lexikální
analýzu Slangu kvůli jeho rychlosti. Nakonec byla ale jeho integrace se zvolenou
knihovnou pro parsování moc složitá.

Z textu generuje AST rovnou ručně napsaný parser pomocí knihovny Megaparsec.
Princip Megaparsec knihovny je skládání menších parserů do větších. @megaparsec
Například parser pro součet je parser, který nejdřív načte levého sčítance, pak
token s významem součtu a nakonec pravého sčítance. Parser má tedy strukturu
rekurzivního sestupu.

Výhodou tohoto systému je možnost vytvoření si jednoduchých parserů pro tokeny
rovnou z textu, takže tokenizaci provádí samotný parser, který tím pádem
ví, kde nastala chyba, a může vytvářet lepší chybové hlášky. Když jsou tyto kroky
rozdělené, tak je mnohem obtížnější si uchovat původní lokaci tokenů ve
zdrojovém kódu.

== Statická analýza

Dalším krokem v překladu zdrojového kódu bývá statická analýza syntaktického
stromu. Jde především o kontrolu, že operace dávají smysl, že existují pro dané
typy a že existují volané funkce, nebo použité proměnné. Často se ke všem
vrcholům stromu také přidá typ výsledku operace. @microc

Překladač Slangu statickou analýzu provádí rovnou při generování kódu. @my-dream
Kód překladače je pak mnohem kratší, protože se neduplikuje struktura
jednotlivých vrcholů stromu a zároveň se lépe dohledávají chyby. Nesměl ji ale
také vynechat, protože u všech LLVM instrukcí je nutné specifikovat typ
argumentů. To pomáhá zejména při optimalizaci a případném ladění. @llvmref

== Generování kódu

Zdaleka nejtěžší část je konverze AST do instrukcí. Naštěstí jsem mohl využít
LLVM, takže jsem nemusel dohledávat rozdíly mezi architekturami procesorů, nebo
generovat validní binární aplikace.

Vstup pro generátor kódu je několik stromů, každý reprezentující jeden stream.
Generátor ze všech definicí streamů a deklarací funkcí extrahuje jejich typy
argumentů a návratových hodnot a vytvoří globální seznam identifikátorů s odkazy
na jednotlivé funkce. Instrukce pro každý stream jsou pak generovány kompletně
odděleně, jenom s kontextem globálních funkcí. 

#figure(
  ```asm
  @str2 = private constant [14 x i8] c"Hello world!\0A\00", align 1

  %stream_main_locals = type {}
  define i1 @stream_main(ptr %l, ptr %rp, %clt* %cl, i8** %b) noinline {
    %1 = load ptr, ptr %b
    indirectbr ptr %1, [ label %.block0, label %.block1, label %.blockblocked ]

  .block0:
    %2 = call i32 (ptr, ...) @printf(ptr @str2)
    %3 = trunc i64 0 to i32
    store i32 %3, ptr %rp
    store i8* blockaddress(@stream_main, %.block1), i8** %b
    ret i1 1

  .block1:
    br label %.block0

  .blockblocked:
    store i8* blockaddress(@stream_main, %.blockblocked), i8** %b
    ret i1 0
  }
  ```,
  caption: [LLVM kód streamu `main`, který vypíše "Hello World!"],
  numbering: none,
  supplement: ""
)

=== Struktura LLVM kódu streamu

// TODO: add reference to runtime
Vzhledem k tomu, že streamy nejsou jednoduché funkce, tak se také nemohou stejně
volat. Musí si vždy pamatovat své lokální proměnné i po tom, co se jako funkce
ukončí. Všechna paměť alokovaná pomocí instrukce ```asm alloca``` je po návratu
funkce smazána #footnote[ Paměť není _skutečně_ smazána, ale je volná k použití,
takže ji může jakákoliv jiná funkce přepsat. Vzhledem k tomu, že je paměť mimo
přímou kontrolu funkce, která jí alokovala, můžeme jí považovat za smazanou.] a
je tedy nutné, aby funkci byla alokovaná paměť předána už při jejím volání.
Všechna paměť je tím pádem alokována hned při startu programu ve skutečné
_funkci_ `main` (nikoliv _streamu_, viz @runtime). Každý stream si také musí
pamatovat místo, kde naposledy skončil, aby poté mohl pokračovat. 

Při generování streamu je tedy potřeba také vygenerovat typ se všemi jeho
lokálními proměnnými. V ukázce LLVM kódu se jedná o typ
```asm %stream_main_locals```#footnote[Ten je prázdný, protože stream nemá žádné
lokální proměnné.], který dostane funkce jako první parametr. Blok, kde má stream
pokračovat, se ukládá do speciální proměnné (v ukázce ```asm i8** %b```).

=== Volání kolon <kolony>

Při volání kolony se streamy spouští od konce. Výstup z kolony je poté jednoduše
přístupný, ale bohužel se komplikuje předání vstupní hodnoty.

Nejdříve je potřeba znát z jakých streamů se kolona skládá a jak jdou po sobě.
Pro inicializaci kolony se v kódu musí vygenerovat konstanta s ukazateli na
všechny funkce v koloně. Také se vytvoří speciální typ, který v má v sobě
seřazené všechny typy lokálních proměnných streamů. Alokace lokálních proměnných
se tedy děje při inicializaci celé kolony. Streamy si pak jen předávají
ukazatele na část alokované paměti s jejich proměnnými.

#figure(
  ```asm
  @pipeline_1 = internal constant [3 x %clt*] [
    %clt* @stream_double,
    %clt* @stream_double,
    %clt* @const_copy
  ]
  ; %sptr a i64 jsou vstupem pro @const_copy
  %pipeline_1_stack = type {%stream_double_locals,
    %stream_double_locals, %sptr, i64}

  ; kombinovaný typ s lok. prom. kolony a místem pro ukazatele
  ; bloků, ve kterých mají streamy pokračovat
  %pipeline_1_stack_comb = type { %pipeline_1_stack, [3 x i8*] }
  ```,
  caption: [LLVM kód kolony z #ref(<quadruple>, supplement: "ukázky")],
  numbering: none,
  supplement: ""
)

Aby mohla kolona dostat vstup, je na konec přidán (ve zdrojovém kódu technicky
na začátek) speciální stream ```asm @const_copy``` (viz @runtime), který chápe
lokální proměnné jako vstup a jen ho překopíruje do svého výstupu. Poté se hned
zablokuje. Žádný stream tedy není v koloně poslední a jejich kód může být vždy
stejný. Svůj vstup buď dostává z jiného streamu, nebo z ```asm @const_copy```.

Vrácená hodnota z funkce streamu je boolean, který značí, jestli volaný stream
vrátil hodnotu (jestli nebyl zablokovaný). Podle toho se volající stream
rozhodne, jestli bude pokračovat dál, nebo jestli se také ukončí.

=== Runtime <runtime>

Aby se přeložený program mohl spustit, musí se zavolat funkce `main`.
#footnote[Ještě před zavoláním main funkce se spouští inicializační kód, který
pro Slang definují knihovny `/lib/crt1.o` a `/lib/crti.o`.] Vstupní bod programu
napsaného ve Slangu je ale stream a tím pádem je potřeba k vygenerovanému kódu
funkci přidat. Runtime se tedy stará o inicializaci a poté finální ukončení
programu. Alokuje všechnu paměť, spustí hlavní kolonu (skládající se ze streamu
`main` a `@const_copy`) a předá hodnoty `argc` a `argv`. Nakonec vyzvedne
výstupní hodnotu `main` a vrátí ji jako chybný kód.

V runtime se také nachází stream `@const_copy`, protože je potřeba vždy už při
inicializaci. Runtime zůstává vždy pro všechny programy stejný.

#pagebreak()

= Podněty k dalšímu vývoji

// nice to have funkce
Do Slangu bych určitě rád přidal ještě několik funkcí. Většinou se spíše jedná o
zjednodušení něčeho, co už ve Slangu udělat lze. Myslím, že kritické funkce už
Slang umí.

Například by bylo dobré přidat známý příkaz ```c break```, jednak pro brzké
ukončení smyčky, ale také pro ukončení streamu. Po příkazu by se stream hned
zablokoval. K lepší logice zablokování by také mělo patřit podmíněné chytání z
kolon například pomocí ```slg if let```, nebo ```slg while let```.

#kod(
  "Lepší logika zablokování streamů",
  ```slg
  stream jedna () -> i64 {
    1 | out;
    break;
  }

  stream ukazka () -> () {
    let kolona = () | jedna;
    let jedna = catch kolona;

    if let dalsi = catch kolona {
      printf("dostal jsem 1 dvakrát\n");
    }

    () | out;
  }
  ```
)

Určitě by také bylo dobré mít podporu pro definované struktury složených
z více typů. Toto není nezbytná funkce, jelikož Slang podporuje anonymní n-tice,
ale pojmenované struktury by pro vývojáře byly určitě ergonomičtější.

Další nedostatek je separátní proměnná pro pamatování si místa, kde stream
skončil. Bylo by lepší si tuto informaci uchovat rovnou vedle lokálních
proměnných. Inicializace kolon by také byla o něco rychlejší.

Jako jednu z výhod streamů ve Slangu jsem bral jejich oddělenost a možnost
paralelizace. S aktuálně generovaným LLVM kódem se kolony rozhodně paralelizovat
nedají a aby se dali, musel by se kompletně změnit systém jejich volání.

= Instalace

Instalace překladače a jeho spuštění je popsáno v souboru `README.md` v
repozitáři projektu.

= Závěr

Úspěšně jsem vytvořil překladač pro vlastní jazyk Slang a myslím, že jsem tím
pádem zadání splnil. I přes to má Slang jasné cesty, kudy by se mohl dál
vyvíjet. V průběhu práce jsem se naučil jak fungují překladače a jak vypadá
program na úrovni instrukcí. Programování v Haskellu bylo také velmi zajímavé a
naučilo mě operovat v paradigmatu funkcionálního programování.

#pagebreak()

#outline(title: "Seznam ukázek kódu v jazyce Slang", target: figure.where(kind: "code"))

#bibliography(
  "literature.bib", 
  title: "Odkazy", 
  style: "ieee-no-quotes.csl")

