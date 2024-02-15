{
module Lexer (alexScanTokens, Token(..), TWithRaw(..), pos, tok, raw, wspc, joinWhitespaceToks, AlexPosn(AlexPn)) where

}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  \n                             { lift NewLine }
  $white+                        { \p s -> lift (White s) p s }
  \#[^\n]*                       { \p s -> lift (Comment s) p s }
  let                            { lift Let }

  stream                         { lift Stream }

  rcv                            { lift Rcv }
  send                           { lift Send }
  catch                          { lift Catch }

  if                             { lift If }
  while                          { lift While }
  for                            { lift For }
  
  $digit+                        { \p s -> lift (Int (read s)) p s }
  $digit+\.$digit+               { \p s -> lift (Float (read s)) p s }

  \.                             { lift Dot }
  \,                             { lift Comma }
  \:                             { lift Colon }

  \<                             { lift Lsr }
  \>                             { lift Gtr }
  ==                             { lift Eq }
  !=                             { lift Neq }
  \>=                            { lift Geq }
  \<=                            { lift Leq }

  &&                             { lift And }
  \|\|                           { lift Or }

  =                              { lift Assign }

  \+\+                           { lift Incr }
  \-\-                           { lift Decr }

  [\-\+\*\/\%]                   { \p s -> lift (Operate (head s)) p s }

  \|                             { lift Pipe }
  \;                             { lift Semicolon }

  \(                             { lift LParen }
  \)                             { lift RParen }
  \{                             { lift LBracket }
  \}                             { lift RBracket }
  \[                             { lift RBrace }
  \]                             { lift RBrace }

  \-\>                           { lift Arrow }

  \" ($printable # \")* \"       { \p s -> lift (String $ (init . tail) s) p s }

  $alpha [$alpha $digit \_ \']*  { \p s -> lift (Name s) p s }

{
lift = TWithRaw ""

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

  | Int Int
  | Float Float

  | Dot
  | Comma
  | Colon

  | Lsr
  | Gtr
  | Eq
  | Neq
  | Geq
  | Leq

  | And
  | Or

  | Assign

  | Incr
  | Decr

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

  | String String

  | Name String

  | NewLine
  | White String
  | Comment String
  deriving (Eq, Ord, Show)


data TWithRaw = TWithRaw {
  wspc :: String,
  tok :: Token,
  pos :: AlexPosn,
  raw :: String
} deriving (Eq, Ord, Show)

isJoinable :: Token -> Bool
isJoinable NewLine = True
isJoinable (White _) = True
isJoinable (Comment _) = True
isJoinable _ = False

isTokenJoinable :: TWithRaw -> Bool
isTokenJoinable t = isJoinable (tok t)

joinIfJoinable :: [TWithRaw] -> TWithRaw -> [TWithRaw]
joinIfJoinable (prev:tokens) new =
  if isTokenJoinable prev
    then (newTokWith (raw prev)):tokens
    else new:prev:tokens
  where
    newTokWith s = TWithRaw (s ++ (wspc new)) (tok new) (pos new) (raw new)
    
joinIfJoinable [] new = [new]

joinWhitespaceToks :: [TWithRaw] -> [TWithRaw]
joinWhitespaceToks = reverse . dropWhile isTokenJoinable . foldl joinIfJoinable []
      
}
