module Diamond
    ( diamond
    )
where

alphabet :: String
alphabet = ['A' .. 'Z']

diamond :: Char -> Maybe [String]
diamond c | c `notElem` alphabet = Nothing
          | otherwise = Just (halfDiamond ++ tail (reverse halfDiamond))
  where
    quantity    = getDiamondDiag c
    halfDiamond = [ makeString x quantity | x <- ['A' .. c] ]

getDiamondDiag :: Char -> Int
getDiamondDiag c = length [ x | c `elem` alphabet, x <- ['A' .. c] ]

makeString :: Char -> Int -> String
makeString _ 1 = "A"
makeString c i | c `notElem` alphabet = ""
               | otherwise            = reverse mapping ++ tail mapping
    where mapping = map (\x -> if x /= c then ' ' else c) (take i alphabet)
