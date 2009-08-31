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
           | GrToken
           | LsToken
           | GrEqToken
           | LsEqToken
           | AssignToken            -- =
           | IdentifierToken String -- Atom
           | ThreadToken            -- "thread"
           | StateToken             -- "state"
           | IfToken                -- "if"
           | ThenToken              -- "then"
           | ElseToken              -- "else"
           | EndToken               -- "end"
           | SemicolonToken         -- ;
           | WhereToken             -- "where"
           | WhileToken             -- "while"
           | DoToken
           | BreakToken             -- "break"
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
    , ("THEN", ThenToken)
    , ("WHILE", WhileToken)
    , ("DO", DoToken)
    , ("BREAK", BreakToken)
    , ("ПОТОК", ThreadToken)
    , ("СОСТОЯНИЕ", StateToken)
    , ("ЕСЛИ", IfToken)
    , ("ИНАЧЕ", ElseToken)
    , ("КОНЕЦ", EndToken)
    , ("ГДЕ", WhereToken)
    , ("ТОГДА", ThenToken)
    , ("ПОКА", WhileToken)
    , ("ДЕЛАЙ", DoToken)
    , ("ПРЕРВАТЬ", BreakToken)]


symbolTokens :: [(String, Token)]
symbolTokens = [ ("+", PlusToken)
               , ("-", MinusToken)
               , ("*", AsteriskToken)
               , ("/", SlashToken)
               , ("(", LParToken)
               , (")", RParToken)
               , ("==", EqualToken)
               , ("=", AssignToken)
               , (";", SemicolonToken)
               , (">", GrToken)
               , ("<", LsToken)
               , (">=", GrEqToken)
               , ("<=", LsEqToken)]