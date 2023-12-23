{
module Lexer (alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  \#.*                           ;
  let                            { \_ -> Let }

  stream                         { \s -> Stream }

  rcv                            { \s -> Rcv }
  send                           { \s -> Send }
  catch                          { \s -> Catch }

  if                             { \s -> If }
  while                          { \s -> While }
  for                            { \s -> For }
  
  \.                             { \s -> Dot }
  \,                             { \s -> Comma }

  \<                             { \s -> Lsr }
  \>                             { \s -> Gtr }
  ==                             { \s -> Eq }
  !=                             { \s -> Neq }
  \>=                            { \s -> Geq }
  \<=                            { \s -> Leq }

  &&                             { \s -> And }
  \|\|                             { \s -> Or }

  =                              { \s -> Assign }

  [\-\+\*\/\%]                     { \s -> Operate (head s) }

  \|                             { \s -> Pipe }
  \;                             { \s -> Semicolon }

  \(                             { \s -> LParen }
  \)                             { \s -> RParen }
  \{                             { \s -> LBracket }
  \}                             { \s -> RBracket }
  \[                             { \s -> RBrace }
  \]                             { \s -> RBrace }

  \-\>                           { \s -> Arrow }

  $digit+                        { \s -> Int (read s) }
  \" ($printable # \")* \"       { \s -> String $ (init . tail) s }

  $alpha [$alpha $digit \_ \']*  { \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Let

  | Stream

  | Rcv
  | Send
  | Catch

  | If
  | While
  | For

  | Dot
  | Comma

  | Lsr
  | Gtr
  | Eq
  | Neq
  | Geq
  | Leq

  | And
  | Or

  | Assign

  | Operate Char

  | Pipe
  | Semicolon

  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace

  | Arrow

  | Int Int
  | String String

  | Var String
  deriving (Eq, Show)
}
