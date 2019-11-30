module Prime (nth) where

primes :: [Integer]
primes = [x | x <- [1..], factors x == [1, x]]

factors :: Integer -> [Integer]
factors 0 = []
factors x = [y | y <- [1..(abs x)] , x `rem` y == 0]

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (primes !! (n - 1))