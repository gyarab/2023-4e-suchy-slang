#let fg = rgb("#FEFEFE")
#let bg = rgb("#1A1A1A")
#let gray = rgb("#CECECE")

#set text(fill: fg, font: "Source Code Pro", size: 18pt)
#set raw(syntaxes: "slang-hi.yaml", theme: "HC.tmTheme")
#set page(paper: "presentation-4-3", fill: bg)

#grid(columns: (1fr, 1fr),
  [
    #v(1fr)
    #set text(size: 15pt)
    ```slang
                 extern fn printf
            (&char, ...) -> i32;stream
         delitele i64 -> i64{let n = ///
        catch in;let i = 2;while i*i <= n
       {if n/i*i              == n {i|out;
      }i = i + 1;               }n | out; }
      stream pcsl                () -> i64{
      let i = 2;//
       while true {let
        d = catch i | delitele
          ;if d == i { i | out; } i=
             i + 1; } } stream main (
                 i32, &&char) -> i32 { //
                        let p = () | pcsl;
                              let x = 1;/**/
     while x <=                  1000000 {//
    x = catch p;                printf("%d\n",
     x); }  0 as                i32 | out;}/*
      ==============-:......:=============.
        =%   Programovaci jazyk Slang  %=
          .=#       Adam Suchý       #=.
             .:=*#%%   4  E   %%#+=:
                     .......*/
    ```
    #v(1fr)
  ],
  align(horizon + center,[
    #text(size: 22pt)[*Programovací jazyk Slang*]
    #v(1em)
    #text(size: 18pt, [Adam Suchý, 4.E])
    #v(1em)
    #text(size: 16pt, fill: gray, [https://s.bain.cz/2024-slang])
  ])
)

#pagebreak()

#align(horizon + center, block(align(left, [
    ```slang
    extern fn printf (&char, ...) -> i32;
    ```
    ```slang
    stream main (i32, &&char) -> i32 {
      printf("Hello World!\n");
      0 as i32 | out;
    }
    ```
])))

#set page(margin: 1em)

#grid(columns: (2fr, 1fr), [
  #v(1fr)
  #align(center, block(align(left, [
      ```slang
      extern fn printf (&char, ...) -> i32;

      stream nums () -> i64 {
        let a = 0;
        while true { (a = a + 1) | out; }
      }
      stream fizzbuzz i64 -> (&char, i64) {
        let i = catch in;
        let c: &char;
        if i % 15 == 0 { c = "FizzBuzz\n"; }
        else if i % 3 == 0 { c = "Fizz\n"; }
        else if i % 5 == 0 { c = "Buzz\n"; }
        else { c = "%ld\n"; }
        (c, i) | out;
      }
      stream print (&char, i64) -> () {
        let i = catch in;
        printf(i.0, i.1);
      }

      stream main (i32, &&char) -> i32 {
        catch () | nums | fizzbuzz | print;
      }
      ```
  ])))
  #v(1fr)
], [
  #v(1fr)
  ```
  1
  2
  Fizz
  4
  Buzz
  Fizz
  7
  8
  Fizz
  Buzz
  11
  Fizz
  13
  14
  FizzBuzz
  16
  17
  Fizz
  19
  Buzz
  Fizz
  22
  ...
  ```
  #v(1fr)
])

#set page(margin: auto)

#align(horizon + center, text(size: 24pt,[*Překladač*]))

#pagebreak()

#v(1em)

#align(center, text(size: 18pt,
  [#underline[*LEXER*] -> PARSER -> ANALYZÉR -> CODEGEN]
))

#v(2em)

#align(center, [
  ```slang
  // vstupní bod
  stream main (i32, &&char) -> i32 {
    printf("hello!\n");
    0 as i32 | out;
  }
  ```
  ```
  |
  v
  ```
  #box(width: 60%,
  ```slang
  STREAM, (IDENTIFIER "main"), LPAREN, (IDENTIFIER "i32"), COMMA, AMPERSAND, AMPERSAND, (IDENTIFIER "char"), ARROW, (IDENTIFIER "i32"), LBRACE, ..
  ```
  )
])

#pagebreak()

#v(1em)

#align(center, text(size: 18pt,
  [LEXER -> #underline[*PARSER*] -> ANALYZÉR -> CODEGEN]
))

#v(1em)

#align(center, [
  ```
  |
  v
  ```
  #set text(size: 17pt)
  ```slang
              ┌─────────────┐              
              │STREAM "main"│              
              └──┬──────┬───┘              
                 │      │                  
          ┌─Příkaz 1    └──Příkaz 2        
          │                      │         
┌─────────▼────────┐      ┌──────▼───────┐ 
│ ZAVOLEJ "printf" │      │POŠLI DO "out"│ 
└─────────┬────────┘      └───────┬──────┘ 
          │                       │        
      Argument 1        ┌─────────▼───────┐
          │             │PŘETYPUJ NA "i32"│
┌─────────▼───────┐     └───────┬─────────┘
│STRING "hello!\n"│             │          
└─────────────────┘         ┌───▼───┐      
                            │INT64 0│      
                            └───────┘
  ```
])
#pagebreak()

#v(1em)

#align(center, text(size: 18pt,
  [LEXER -> PARSER -> #underline[*ANALYZÉR*] -> CODEGEN]
))

#v(1em)

#align(center, [
  ```
  |
  v
  ```
  #set text(size: 17pt)
  ```slang
              ┌──────────────────────────────────┐    
              │STREAM "main" (i32, &&char) -> i32│    
              └──┬──────┬────────────────────────┘    
                 │      │                             
          ┌─Příkaz 1    └──Příkaz 2                   
          │                      │                    
┌─────────▼─────────────┐    ┌───▼─────────────────┐  
│ ZAVOLEJ "printf" (i32)│    │POŠLI DO "out" (void)│  
└─────────┬─────────────┘    └──────┬──────────────┘  
          │                         │                 
      Argument 1             ┌──────▼────────────────┐
          │                  │PŘETYPUJ NA "i32" (i32)│
┌─────────▼───────────────┐  └───────┬───────────────┘
│STRING "hello!\n" (&char)│          │                
└─────────────────────────┘      ┌───▼─────────┐      
                                 │INT64 0 (i64)│      
                                 └─────────────┘      
  ```
])

// #set page(margin: 1em)
#pagebreak()

#v(1em)

#align(center, text(size: 18pt,
  [LEXER -> PARSER -> ANALYZÉR -> #underline[*CODEGEN*]]
))

#v(1em)

#align(center, [
  ```
  |
  v
  ```
  #set text(size: 16.5pt)
  ```asm
  define i1 @stream_main(ptr %l, ptr %return_ptr, %clt* %c, i8** %block) noinline {
    %1 = load ptr, ptr %block
    indirectbr ptr %1, [ label %.block0, label %.block1, label %.blockblocked ]

  .block0:
    %2 = call i32 (ptr, ...) @printf(ptr @str1)
    %3 = trunc i64 0 to i32
    store i32 %3, ptr %return_ptr
    store i8* blockaddress(@stream_main, %.block1), i8** %block
    ret i1 1

  .block1:
    br label %.block0

  .blockblocked:
    store i8* blockaddress(@stream_main, %.blockblocked), i8** %block
    ret i1 0
  }
  ```
])

#pagebreak()

#align(horizon + center, text(size: 24pt,[*Ukázka*]))
