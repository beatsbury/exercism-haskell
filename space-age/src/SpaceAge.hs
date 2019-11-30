module SpaceAge
    ( Planet(..)
    , ageOn
    )
where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq)

type Seconds = Float
earthYear :: Seconds
earthYear = 60 * 60 * 24 * 365.25   --31557600

orbitalPeriod :: Planet -> Float
orbitalPeriod p | p == Mercury = 0.2408467
                | p == Venus   = 0.61519726
                | p == Mars    = 1.8808158
                | p == Jupiter = 11.862615
                | p == Saturn  = 29.447498
                | p == Uranus  = 84.016846
                | p == Neptune = 164.79132
                | otherwise    = 1.0

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (orbitalPeriod planet * earthYear)
