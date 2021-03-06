module Day6Solution
  ( solveA
  , solveB
  ) where

import           Control.Monad
import           Data.List.Split
import           Data.Maybe
import           Data.MultiMap   (empty, fromList, lookup)
import           Data.Tree
import           Prelude         hiding (lookup)

toTree input =
  let forest = (,) `ap` flip lookup (fromList input)
   in unfoldTree forest "COM"

parse =
  let toTuple [body, satellite] = (body, satellite)
   in toTree . fmap (toTuple . splitOn ")") . lines

depths = depths' 0
  where
    depths' depth (Node _ []) = depth
    depths' depth (Node _ forest) = depth + sum (depths' (depth + 1) <$> forest)

solveA = depths . parse

data Found
  = Nobody
  | You Int
  | Santa Int
  | Both Int

result (Both x) = x

found "YOU" _ = You 0
found "SAN" _ = Santa 0
found _ [] = Nobody
found _ forest =
  let found' Nobody Nobody     = Nobody
      found' (Santa x) (You y) = Both $ x + y - 1
      found' (You y) (Santa x) = Both $ x + y - 1
      found' (Santa x) _       = Santa x
      found' _ (Santa x)       = Santa $ x + 1
      found' (You x) _         = You x
      found' _ (You x)         = You $ x + 1
      found' (Both x) _        = Both x
      found' _ (Both x)        = Both x
   in foldl found' Nobody forest

distance = result . foldTree found

solveB = distance . parse
