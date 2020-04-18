module Dominion.Log.Parse where

import Prelude

newtype Card = Card String
newtype Player = Player String

data DeckUpdate
  = Gain Player Card
  | Draw Player Card
  | Trash Player Card

parseLine :: String -> Array DeckUpdate
parseLine _ = []
