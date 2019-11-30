module Anagram (anagramsFor) where

import Data.Char(toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor string candidates = [anagram | anagram <- candidates, isAnagram string anagram]

isAnagram :: String -> String -> Bool
isAnagram xs ys | lowerCase xs == lowerCase ys = False
                | otherwise = sortString (lowerCase xs) == sortString (lowerCase ys)
                where lowerCase = map toLower

sortString :: String -> String
sortString [] = []
sortString (x:xs) = sortString smallerThans ++ [x] ++ sortString greaterThans
    where   smallerThans = [y | y <- xs, y < x]
            greaterThans = [z | z <- xs, z >= x]