module Day6Solution
  ( solveA
  ) where

import           Data.Tree

test =
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

parse _ = test

depths _ forest = 0 : fmap (+1) (concat forest)

solveA = toInteger . sum . foldTree depths . parse
