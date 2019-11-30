module SecretHandshake (handshake) where

--1 = wink                                                              1
--10 = double blink                                                     2
--100 = close your eyes                                                 4
--1000 = jump                                                           8
--10000 = Reverse the order of the operations in the secret handshake.  16

handshake :: Int -> [String]
handshake x
    | x < 0 = error "Secret number cannot be negative"
    | isReverse x = reverse (map represent (getNumRep x 16))
    | otherwise = map represent (getNumRep x 16)

represent :: Int -> String
represent x
    | x == 1 = "wink"
    | x == 2 = "double blink"
    | x == 4 = "close your eyes"
    | x == 8 = "jump"
    | otherwise = "do arbitrary stuff"

isReverse :: Int -> Bool
isReverse x
    | x >= 16 = (x `div` 16) `rem` 2 > 0
    | otherwise = False

getNumAction :: Int -> Int -> [Int]
getNumAction _ 0 = []
getNumAction 0 _ = []
getNumAction x y
    | x >= y = y : getNumAction (x - y) y
    | otherwise = []

getNumRep :: Int -> Int -> [Int]
getNumRep _ 0 = []
getNumRep 0 _ = []
getNumRep x y =
    getNumRep (x `rem` y) (y `div` 2) ++ getNumAction (x `rem` y) (y `div` 2)