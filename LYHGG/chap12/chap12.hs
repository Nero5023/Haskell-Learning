import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare str0 str1 = (length str0 `compare` length str1) `mappend` 
                          (vowels str0 `compare` vowels str1) `mappend`
                          (str0 `compare` str1) 
           where vowels = length . filter (`elem` "aeiou")