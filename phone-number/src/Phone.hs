module Phone (number) where

number :: String -> Maybe String
--number xs = error "You need to implement this function."
number [] = Nothing
number xs =
    if length cleanedNumber == 10 then Just cleanedNumber
    else Nothing
    where cleanedNumber = verify (clean xs) 

clean :: String -> String
clean [] = []
clean xs = [y | y <- xs, y `elem` "0123456789"]

verify :: String -> String
verify xs 
    | length xs == 11 && head xs == '1' = verify (tail xs)
    | length xs == 10 && head xs `elem` checkList && xs !! 3 `elem` checkList = xs
    | otherwise = []
    where checkList = "23456789"