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

distance = tupleDistance . distance' 0
  where
    tupleDistance (Just x, Just y) = x + y
    distance' depth (Node "YOU" _) = (Just depth, Nothing)
    distance' depth (Node "SAN" _) = (Nothing, Just depth)
    distance' depth (Node _ []) = (Nothing, Nothing)
    distance' depth (Node _ satellites) =
      let pairs = distance' (depth + 1) <$> satellites
          m (Just x, Just y) _                  = (Just x, Just y)
          m _ (Just x, Just y)                  = (Just x, Just y)
          m (Just x, Nothing) (Nothing, Just y) = (Just (x - depth - 1), Just (y - depth - 1))
          m (Nothing, Just y) (Just x, Nothing) = (Just (x - depth - 1), Just (y - depth - 1))
          m (Just x, Nothing) _                 = (Just x, Nothing)
          m _ (Nothing, Just y)                 = (Nothing, Just y)
          m _ (Just x, Nothing)                 = (Just x, Nothing)
          m (Nothing, Just y) _                 = (Nothing, Just y)
          m _ _                                 = (Nothing, Nothing)
       in foldl m (Nothing, Nothing) pairs

solveB = toInteger . distance . parse
