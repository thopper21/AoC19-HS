module Day1
  ( solveA
  , solveB
  ) where

import           Data.Monoid

fuelRequirement mass = (mass `div` 3) - 2

fullFuelRequirement mass =
  let fuelMass = fuelRequirement mass
      remainder =
        if fuelMass > 0
          then max (fullFuelRequirement fuelMass) 0
          else 0
   in fuelMass + remainder

solve fn input = getSum $ mconcat $ Sum . fn . read <$> lines input

solveA = solve fuelRequirement

solveB = solve fullFuelRequirement
