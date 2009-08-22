module Language.Rfx.Tokens(Token(..), tokenStrings)
where

data Token = NumberToken Int        -- Number
           | PlusToken              -- +
           | MinusToken             -- -
           | LParToken              -- (
           | RParToken              -- )
           | EqualToken             -- ==
           | AssignToken            -- =
           | IdentifierToken String -- Atom
           | ThreadToken            -- "thread"
           | StateToken             -- "state"
           | IfToken                -- "if"
           | ElseToken              -- "else"
           | EndToken               -- "end"
           | SemicolonToken         -- ;
           | EOFToken
             deriving (Show, Eq)

tokenStrings :: [(String, Token)]
tokenStrings =
    [("+", PlusToken)
     ,("-", MinusToken)
     ,("(", LParToken)
     ,(")", RParToken)
     ,("==", EqualToken)
     ,("=", AssignToken)
     ,("THREAD", ThreadToken)
     ,("STATE", StateToken)
     ,("IF", IfToken)
     ,("ELSE", ElseToken)
     ,("END", EndToken)
     ,(";", SemicolonToken)]