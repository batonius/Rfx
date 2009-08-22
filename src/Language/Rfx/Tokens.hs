module Language.Rfx.Tokens(Token(..), keywordTokens, symbolTokens)
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

keywordTokens :: [(String, Token)]
keywordTokens =
    [ ("THREAD", ThreadToken)
    , ("STATE", StateToken)
    , ("IF", IfToken)
    , ("ELSE", ElseToken)
    , ("END", EndToken)]


symbolTokens :: [(String, Token)]
symbolTokens = [ ("+", PlusToken)
               , ("-", MinusToken)
               , ("(", LParToken)
               , (")", RParToken)
               , ("==", EqualToken)
               , ("=", AssignToken)
               , (";", SemicolonToken)]