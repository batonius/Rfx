module Language.Rfx.Util(tlString)
where

tlString :: String -> String
tlString = concat .  map transliterateChar

-- quick'n'dirty                      
transliterateChar :: Char -> String
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
transliterateChar 'Ъ' = ""
transliterateChar 'Ы' = "I"
transliterateChar 'Ь' = ""
transliterateChar 'Э' = "A"
transliterateChar 'Ю' = "U"
transliterateChar 'Я' = "YA"
transliterateChar a = [a]