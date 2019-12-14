module Day1Solution
  ( solveA
  , solveB
  ) where

import           Data.Monoid

fuelRequirement mass = max ((mass `div` 3) - 2) 0

fullFuelRequirement mass =
  let fuelMass = fuelRequirement mass
      remainder =
        if fuelMass > 0
          then fullFuelRequirement fuelMass
          else 0
   in fuelMass + remainder

solve fn input = getSum $ mconcat $ Sum . fn . read <$> lines input

solveA = solve fuelRequirement

solveB = solve fullFuelRequirement
