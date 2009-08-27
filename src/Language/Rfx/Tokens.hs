module Language.Rfx.Tokens(Token(..), keywordTokens, symbolTokens)
where

data Token = NumberToken Int        -- Number
           | PlusToken              -- +
           | MinusToken             -- -
           | AsteriskToken          -- *
           | SlashToken             -- /
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
           | WhereToken             -- "where"
           | BoolToken
           | Int8Token
           | EOFToken
             deriving (Show, Eq)

keywordTokens :: [(String, Token)]
keywordTokens =
    [ ("THREAD", ThreadToken)
    , ("STATE", StateToken)
    , ("IF", IfToken)
    , ("ELSE", ElseToken)
    , ("END", EndToken)
    , ("WHERE", WhereToken)
    , ("ПОТОК", ThreadToken)
    , ("СОСТОЯНИЕ", StateToken)
    , ("ЕСЛИ", IfToken)
    , ("ИНАЧЕ", ElseToken)
    , ("КОНЕЦ", EndToken)
    , ("ГДЕ", WhereToken)]


symbolTokens :: [(String, Token)]
symbolTokens = [ ("+", PlusToken)
               , ("-", MinusToken)
               , ("*", AsteriskToken)
               , ("/", SlashToken)
               , ("(", LParToken)
               , (")", RParToken)
               , ("==", EqualToken)
               , ("=", AssignToken)
               , (";", SemicolonToken)]