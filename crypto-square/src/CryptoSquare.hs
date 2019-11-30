module CryptoSquare (encode) where

import Data.Char(toLower)

alphabet :: String
alphabet = ['A'..'Z'] ++ ['a'..'z']

encode :: String -> String
encode xs = error "You need to implement this function."

normalize :: String -> String
normalize = map toLower . filter (`elem` alphabet)

dimensions :: Int -> (Int, Int)
dimensions 0 = (0, 0)
dimensions x = head [(r, c) | r <- [1..x], c <- [r, succ r], r * c >= x, (r * c) - r < x ]

getSquared :: String -> [String]
getSquared [] = []
getSquared xs = fst splitted : getSquared (snd splitted)
    where splitted = splitAt (snd (dimensions (length xs))) xs