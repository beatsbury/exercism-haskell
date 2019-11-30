module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
--classify = error "You need to implement this function."
classify x
    |   x <= 0 = Nothing
    |   factorsSum == x = Just Perfect
    |   factorsSum > x = Just Abundant
    |   factorsSum < x = Just Deficient
        where 
            factorsSum = sum (getFactors x)
classify _ = Nothing

getFactors :: Int -> [Int]
getFactors 0 = []
getFactors x = [y | y <- [1..(abs x - 1)], x `rem` y == 0]

