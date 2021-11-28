module Data.Text.Hematria.Dictionary.Spanish where

replaceSpecialChar :: Char -> Char
replaceSpecialChar 'á' = 'a'
replaceSpecialChar 'é' = 'e'
replaceSpecialChar 'í' = 'i'
replaceSpecialChar 'ó' = 'o'
replaceSpecialChar 'ö' = 'o'
replaceSpecialChar 'ú' = 'u'
replaceSpecialChar 'ü' = 'u'
replaceSpecialChar c = c
