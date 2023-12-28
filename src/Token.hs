module Token where

data TokenType = TokenType
  | Comment
  | LParen
  | RParen
  | FuncCall
  | IntegerLiteral
  | StringLiteral
  | Variable
  | EOF
  deriving (Show, Eq)

data Token = Token
  { tokenValue :: String
  , tokenType :: TokenType
  } deriving (Show)
