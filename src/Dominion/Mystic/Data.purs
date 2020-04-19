module Dominion.Mystic.Data where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))

newtype Card = Card String
derive instance genericCard :: Generic Card _
derive instance eqCard :: Eq Card
instance showCard :: Show Card where
  show x = genericShow x

cardPluralizationExceptions :: Map.Map String String
cardPluralizationExceptions =
  Map.fromFoldable [ same "Nobles"
                   , t "Emporia" "Emporium"
                   ]
  where same name = Tuple name name
        t = Tuple

singleS :: Pattern
singleS = Pattern "s"

cardFromPluralizedName :: String -> Card
cardFromPluralizedName name = Card $ Maybe.fromMaybe sEndingRemoved exception
  where sEndingRemoved = Maybe.fromMaybe name $ String.stripSuffix singleS name
        exception = Map.lookup name cardPluralizationExceptions

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
  | Topdecks Player CardList
  | Reveals Player CardList
  | Returns Player CardList
  | PutsIntoHand Player CardList
  | Plays Player Card
  | LooksAt Player CardList
  | Shuffles Player

derive instance genericDeckUpdate :: Generic DeckUpdate _
derive instance eqDeckUpdate :: Eq DeckUpdate
instance showDeckUpdate :: Show DeckUpdate where
  show x = genericShow x
