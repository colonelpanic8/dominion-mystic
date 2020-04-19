module Dominion.Data where

import Data.Generic.Rep
import Data.Show (class Show)
import Data.Eq (class Eq)
import Data.Generic.Rep.Show (genericShow)

newtype Card = Card String
derive instance genericCard :: Generic Card _
derive instance eqCard :: Eq Card
instance showCard :: Show Card where
  show x = genericShow x

newtype Player = Player String
derive instance genericPlayer :: Generic Player _
derive instance eqPlayer :: Eq Player
instance showPlayer :: Show Player where
  show x = genericShow x

data DeckUpdate
  = Gain Player Card
  | Draw Player Card
  | Trash Player Card
  | Exile Player Card
  | Discard Player Card
