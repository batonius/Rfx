module Language.Rfx.Util(tlString, ProgramPos(..), posChildOf)
where

data ProgramPos = InGlobal
               | InThread String
               | InState String String
                 deriving (Show, Eq, Ord)

-- Does b include a
posChildOf :: ProgramPos -> ProgramPos -> Bool
posChildOf a b = case b of
                 InGlobal -> True
                 InThread btn -> case a of
                                  InThread atn -> atn == btn
                                  InState atn _ -> atn == btn
                                  InGlobal -> False
                 InState _ _ -> a == b

tlString :: String -> String
tlString = concat .  map transliterateChar

-- quick'n'dirty
transliterateChar :: Char -> String
transliterateChar 'а' = "A"
transliterateChar 'б' = "B"
transliterateChar 'в' = "V"
transliterateChar 'г' = "G"
transliterateChar 'д' = "D"
transliterateChar 'е' = "E"
transliterateChar 'ё' = "E"
transliterateChar 'ж' = "ZH"
transliterateChar 'z' = "Z"
transliterateChar 'и' = "I"
transliterateChar 'й' = "Y"
transliterateChar 'к' = "K"
transliterateChar 'л' = "L"
transliterateChar 'м' = "M"
transliterateChar 'н' = "N"
transliterateChar 'о' = "O"
transliterateChar 'п' = "P"
transliterateChar 'р' = "R"
transliterateChar 'с' = "C"
transliterateChar 'т' = "T"
transliterateChar 'у' = "U"
transliterateChar 'ф' = "F"
transliterateChar 'х' = "H"
transliterateChar 'ц' = "C"
transliterateChar 'ч' = "CH"
transliterateChar 'ш' = "SH"
transliterateChar 'щ' = "SH"
transliterateChar 'ъ' = "_"
transliterateChar 'ы' = "I"
transliterateChar 'ь' = "_"
transliterateChar 'э' = "A"
transliterateChar 'ю' = "U"
transliterateChar 'я' = "YA"
transliterateChar 'А' = "A"
transliterateChar 'Б' = "B"
transliterateChar 'В' = "V"
transliterateChar 'Г' = "G"
transliterateChar 'Д' = "D"
transliterateChar 'Е' = "E"
transliterateChar 'Ё' = "E"
transliterateChar 'Ж' = "ZH"
transliterateChar 'З' = "Z"
transliterateChar 'И' = "I"
transliterateChar 'Й' = "Y"
transliterateChar 'К' = "K"
transliterateChar 'Л' = "L"
transliterateChar 'М' = "M"
transliterateChar 'Н' = "N"
transliterateChar 'О' = "O"
transliterateChar 'П' = "P"
transliterateChar 'Р' = "R"
transliterateChar 'С' = "C"
transliterateChar 'Т' = "T"
transliterateChar 'У' = "U"
transliterateChar 'Ф' = "F"
transliterateChar 'Х' = "H"
transliterateChar 'Ц' = "C"
transliterateChar 'Ч' = "CH"
transliterateChar 'Ш' = "SH"
transliterateChar 'Щ' = "SH"
transliterateChar 'Ъ' = "_"
transliterateChar 'Ы' = "I"
transliterateChar 'Ь' = "_"
transliterateChar 'Э' = "A"
transliterateChar 'Ю' = "U"
transliterateChar 'Я' = "YA"
transliterateChar a = [a]