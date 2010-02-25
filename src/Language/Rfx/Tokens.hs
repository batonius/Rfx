module Language.Rfx.Tokens(Token(..), keywordTokens, symbolTokens)
where
import Text.ParserCombinators.Parsec(SourcePos)

data Tagged a = Tagged
    {
      sourcePos :: SourcePos
    , value :: a
    }
                deriving (Show)

instance Eq a => Eq (Tagged a) where
    (Tagged _ x) == (Tagged _ y) = x == y

data Token = NumberToken Int        -- Number
           | StringToken String
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
           | DotToken
           | NextToken
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
           | CommaToken
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
    , ("NEXT", NextToken)
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
    , ("ПРЕРВАТЬ", BreakToken)
    , ("ДАЛЬШЕ", NextToken)]

symbolTokens :: [(String, Token)]
symbolTokens = [ ("+", PlusToken)
               , ("-", MinusToken)
               , ("*", AsteriskToken)
               , ("/", SlashToken)
               , ("(", LParToken)
               , (")", RParToken)
               , (">=", GrEqToken)
               , ("<=", LsEqToken)
               , ("==", EqualToken)
               , ("=", AssignToken)
               , (";", SemicolonToken)
               , (">", GrToken)
               , ("<", LsToken)
               , (".", DotToken)
               , (",", CommaToken)
               ]