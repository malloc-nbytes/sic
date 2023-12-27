module Token where

data TokenType = TokenType
  | Comment
  | LParen
  | RParen
  | Wildcard
  | FuncCall
  | IntegerLiteral
  | Macro
  | StringLiteral
  | EOF
  deriving (Show, Eq)

data Token = Token
  { tokenValue :: String
  , tokenType :: TokenType
  } deriving (Show)