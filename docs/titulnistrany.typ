#set par(leading: 0.55em, first-line-indent: 1.8em, justify: true)
#show par: set block(spacing: 0.55em)
#show heading: set block(above: 1.4em, below: 1em)

#[
  #set page(margin: 0in)
  #set align(center)
  
  #v(1fr)
  
  #set text(18pt)
  
  #strong[Gymnázium Arabská, Praha 6, Arabská 14]
  
  #v(1em)

  #[
    #set text(16pt)
    Obor programování
  ]
  
  #v(2em)
  
  #image("images/logogyarab.png", width: 45%)
  
  #v(2em)
  
  #strong[Programovací jazyk Slang]

  #v(2em)

  #[
    #set text(16pt)

    Adam Suchý
  ]
  
  #v(2em)
  
  #[
    #set text(14pt)
    Duben, 2024
  ]
  
  #v(1fr)
]

#[
  #set par(first-line-indent: 0em)

  #v(1fr)
  
  Zdrojový kód je veřejně dostupný pod licencí MIT. Její plné znění v
  anglickém jazyce je přiloženo v souboru `LICENSE` v repozitáři projektu.

  #v(1em)

  dále:

  #v(1em)

  Prohlašuji, že jsem jediným autorem tohoto projektu, všechny citace jsou
  řádně označené a všechna použitá literatura a další zdroje jsou v práci
  uvedené. Tímto dle zákona 121/2000 Sb. (tzv.~Autorský zákon) ve znění
  pozdějších předpisů uděluji bezúplatně škole Gymnázium, Praha 6, Arabská 14
  oprávnění k výkonu práva na rozmnožování díla (§ 13) a práva na sdělování
  díla veřejnosti (§ 18) na dobu časově neomezenou a bez omezení územního
  rozsahu.

  #v(6em)

  #set align(right)
  
  V #box(width: 7em, repeat[.]) dne #box(width: 8em, repeat[.]) #h(1fr) Adam Suchý #h(1.5em) #box(width: 10em, repeat[.])

  #v(10em)
  
  #pagebreak()
]

#[
  #set page(margin: 8em)
  #set align(center)

  #text(size: 16pt)[ *Anotace* ]
  #v(1em)

  Slang je víceúčelový programovací jazyk založený na principu datových proudů.
  V této práci jsem definoval jeho syntaxi a naprogramoval překladač do LLVM IR.
  Dále vysvětluji postup, jak překladač funguje a jak by vývoj Slangu mohl
  pokračovat.

  #v(2em)
  #text(size: 16pt)[ *Abstract* ]
  #v(1em)

  Slang is a general purpose programming language with first-class support for data streams.
  In this work I define it's syntax and build a compiler into LLVM IR. Additionally,
  I explain how the compiler works and how its future developement could look like.
]
