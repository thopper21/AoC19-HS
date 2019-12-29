module Day6Solution
  ( solveA
  ) where

import           Data.Map
import           Data.Maybe
import           Data.Tree
import           Prelude    hiding (lookup)

testTree =
  Node
    "COM"
    [ Node
        "B"
        [ Node "G" [Node "H" []]
        , Node
            "C"
            [ Node
                "D"
                [ Node "I" []
                , Node "E" [Node "F" [], Node "J" [Node "K" [Node "L" []]]]
                ]
            ]
        ]
    ]

testInput =
  [ ("COM", "B")
  , ("B", "C")
  , ("C", "D")
  , ("D", "E")
  , ("E", "F")
  , ("B", "G")
  , ("G", "H")
  , ("D", "I")
  , ("E", "J")
  , ("J", "K")
  , ("K", "L")
  ]

toTree input =
  let listVal (k, v) = (k, [v])
      map = fromListWith (++) (listVal <$> input)
      forest body = (body, fromMaybe [] $ lookup body map)
   in unfoldTree forest "COM"

parse _ = toTree testInput

depths = depths' 0
  where
    depths' depth (Node _ []) = depth
    depths' depth (Node _ forest) = depth + sum (depths' (depth + 1) <$> forest)

solveA = toInteger . depths . parse
