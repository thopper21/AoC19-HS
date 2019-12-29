module Day6Solution
  ( solveA
  , solveB
  ) where

import           Data.List.Split
import           Data.Map        (empty, fromListWith, lookup)
import           Data.Maybe
import           Data.Tree
import           Prelude         hiding (lookup)

toTree input =
  let listVal (k, v) = (k, [v])
      map = fromListWith (++) (listVal <$> input)
      forest body = (body, fromMaybe [] $ lookup body map)
   in unfoldTree forest "COM"

parse =
  let toTuple [body, satellite] = (body, satellite)
   in toTree . fmap (toTuple . splitOn ")") . lines

depths = depths' 0
  where
    depths' depth (Node _ []) = depth
    depths' depth (Node _ forest) = depth + sum (depths' (depth + 1) <$> forest)

solveA = toInteger . depths . parse

data Found = Nobody | You Int | Santa Int | Both Int

result (Both x) = x

distance (Node "YOU" _) = You 0
distance (Node "SAN" _) = Santa 0
distance (Node _ []) = Nobody
distance (Node _ satellites) =
    let
        found Nobody Nobody = Nobody
        found (Santa x) (You y) = Both $ x + y - 1
        found (You y) (Santa x) = Both $ x + y - 1
        found (Santa x) _ = Santa x
        found _ (Santa x) = Santa $ x + 1
        found (You x) _ = You x
        found _ (You x) = You $ x + 1
        found (Both x) _ = Both x
        found _ (Both x) = Both x
    in foldl found Nobody (distance <$> satellites)

solveB = toInteger . result . distance . parse
