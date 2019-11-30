module Strain
    ( keep
    , discard
    )
where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = [ z | z <- xs, not (p z) ]

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [ z | z <- xs, p z ]
