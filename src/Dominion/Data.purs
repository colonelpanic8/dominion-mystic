module Dominion.Data where

import Data.Generic.Rep

import Data.Eq (class Eq)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)
import Data.Tuple (Tuple)

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

type CardQuantity = Tuple Int Card
type CardList = Array CardQuantity

data DeckUpdate
  = Gains Player CardList
  | Draws Player CardList
  | Trashes Player CardList
  | Exiles Player CardList
  | Discards Player CardList

derive instance genericDeckUpdate :: Generic DeckUpdate _
derive instance eqDeckUpdate :: Eq DeckUpdate
instance showDeckUpdate :: Show DeckUpdate where
  show x = genericShow x
