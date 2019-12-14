module Day1
  ( solveA
  ) where

import           Data.Monoid

fuelRequirement :: Integer -> Integer
fuelRequirement mass = (mass `div` 3) - 2

solveA input = getSum $ mconcat $ Sum . fuelRequirement . read <$> lines input
