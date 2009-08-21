module Language.Rfx.Parser(parseTokens)
where
import Language.Rfx.Tokens
import Language.Rfx.Structures

parseTokens :: [Token] -> Program
parseTokens _ = Program []