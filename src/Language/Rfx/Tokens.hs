module Language.Rfx.Tokens(Token(..))
where

data Token = NumberToken Int
           | PlusToken
             deriving (Show, Eq)
