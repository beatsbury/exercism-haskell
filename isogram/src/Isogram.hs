module Isogram (isIsogram) where

import Data.Char
import Data.List

isIsogram :: String -> Bool
isIsogram [] = True
isIsogram [_] = True
isIsogram string =
    isUnique coString (head coString) && isIsogram (tail coString)
    where coString = charsOnly string

isUnique :: String -> Char -> Bool
isUnique [] _ = False
isUnique string character =
    length [x | x <- string, x == character] == 1

charsOnly :: String -> String
charsOnly string =
    [toLower s | s <- string, s `elem` (['a'..'z'] ++ ['A'..'Z'])]

--Joe Warren's solution
isIsogram' :: String -> Bool
isIsogram' = all ((==1) . length) . group . sort . fmap toLower . filter isAlpha