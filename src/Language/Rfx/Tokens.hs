{-# LANGUAGE NamedFieldPuns #-}
module Language.Rfx.Tokens(Token(..), keywordTokens, symbolTokens, Tagged(..))
where
import Text.ParserCombinators.Parsec(SourcePos)

data Tagged a = Tagged
    {
      sourcePos :: SourcePos
    , value :: a
    }

instance Show a => Show (Tagged a) where
    show Tagged{sourcePos, value} = (show value) ++ " at " ++ (show sourcePos)

instance Eq a => Eq (Tagged a) where
    (Tagged _ x) == (Tagged _ y) = x == y

data Token = NumberToken Int        -- Number
           | StringToken String     -- String
           | TimeToken Integer      -- Time
           | PlusToken              -- +
           | MinusToken             -- -
           | AsteriskToken          -- *
           | SlashToken             -- /
           | LParToken              -- (
           | RParToken              -- )
           | LBracketToken          -- [
           | RBracketToken          -- ]
           | EqualToken             -- ==
           | NEqualToken            -- !=
           | GrToken                -- >
           | LsToken                -- <
           | GrEqToken              -- >=
           | LsEqToken              -- <=
           | DotToken               -- .
           | NextToken              -- "next"
           | WaitToken              -- "wait"
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
           | DoToken                -- "do"
           | ReturnToken            -- "return"
           | CommaToken             -- ,
           | BreakToken             -- "break"
           | TrueToken              -- "true"
           | FalseToken             -- "false"
           | AndToken               -- "and"
           | OrToken                -- "or"
           | XorToken               -- "xor"
           | NotToken               -- "not"
           | VoidToken              -- "void"
           | EOFToken
           | CommentToken
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
    , ("RETURN", ReturnToken)
    , ("TRUE", TrueToken)
    , ("FALSE", FalseToken)
    , ("AND", AndToken)
    , ("OR", OrToken)
    , ("XOR", XorToken)
    , ("NOT", NotToken)
    , ("WAIT", WaitToken)
    , ("ПОТОК", ThreadToken)
    , ("СОСТОЯНИЕ", StateToken)
    , ("ЕСЛИ", IfToken)
    , ("ИНАЧЕ", ElseToken)
    , ("КОНЕЦ", EndToken)
    , ("ГДЕ", WhereToken)
    , ("ТОГДА", ThenToken)
    , ("ПОКА", WhileToken)
    , ("ДЕЛАЙ", DoToken)
    , ("ВЕРНИ", ReturnToken)
    , ("ПРЕРВАТЬ", BreakToken)
    , ("ДАЛЬШЕ", NextToken)
    , ("ИСТИНА", TrueToken)
    , ("ЛОЖЬ", FalseToken)
    , ("И", AndToken)
    , ("ИЛИ", OrToken)
    , ("ИИЛИ", XorToken)
    , ("НЕ", NotToken)]

symbolTokens :: [(String, Token)]
symbolTokens = [ ("+", PlusToken)
               , ("-", MinusToken)
               , ("*", AsteriskToken)
               , ("/", SlashToken)
               , ("(", LParToken)
               , (")", RParToken)
               , ("[", LBracketToken)
               , ("]", RBracketToken)
               , (">=", GrEqToken)
               , ("<=", LsEqToken)
               , ("==", EqualToken)
               , ("!=", NEqualToken)
               , ("=", AssignToken)
               , (";", SemicolonToken)
               , (">", GrToken)
               , ("<", LsToken)
               , (".", DotToken)
               , (",", CommaToken)
               ]