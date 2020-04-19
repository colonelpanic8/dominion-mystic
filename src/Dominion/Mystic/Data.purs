module Dominion.Mystic.Data where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))

newtype Card
  = Card String

derive instance genericCard :: Generic Card _

derive instance eqCard :: Eq Card

instance showCard :: Show Card where
  show x = genericShow x

cardPluralizationExceptions :: Map.Map String String
cardPluralizationExceptions =
  Map.fromFoldable
    [ same "Nobles"
    , t "Emporia" "Emporium"
    ]
  where
  same name = Tuple name name

  t = Tuple

singleS :: Pattern
singleS = Pattern "s"

cardFromPluralizedName :: String -> Card
cardFromPluralizedName name = Card $ Maybe.fromMaybe sEndingRemoved exception
  where
  sEndingRemoved = Maybe.fromMaybe name $ String.stripSuffix singleS name

  exception = Map.lookup name cardPluralizationExceptions

newtype Player
  = Player String

derive instance genericPlayer :: Generic Player _

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
  show x = genericShow x

type CardQuantity
  = Tuple Card Int

type CardList
  = Array CardQuantity

data DeckUpdate
  = Shuffles Player
  | Discards Player CardList
  | Draws Player CardList
  | Exiles Player CardList
  | Gains Player CardList
  | LooksAt Player CardList
  | Plays Player CardList
  | PutsIntoHand Player CardList
  | Returns Player CardList
  | Reveals Player CardList
  | Topdecks Player CardList
  | Trashes Player CardList

derive instance genericDeckUpdate :: Generic DeckUpdate _

derive instance eqDeckUpdate :: Eq DeckUpdate

instance showDeckUpdate :: Show DeckUpdate where
  show x = genericShow x

type CountsByCardType
  = Map.Map Card Int

data Deck
  = Deck
    { discard :: CountsByCardType
    , deck :: CountsByCardType
    , play :: CountsByCardType
    , exile :: CountsByCardType
    , tavern :: CountsByCardType
    , island :: CountsByCardType
    }

derive instance genericDeck :: Generic Deck _

derive instance eqDeck :: Eq Deck

instance showDeck :: Show Deck where
  show x = genericShow x

type PlayerState
  = Deck

data GameState
  = GameState
    { stateByPlayer :: Map.Map Player PlayerState
    , hasCurrentTurn :: Player
    }

derive instance genericGameState :: Generic GameState _

derive instance eqGameState :: Eq GameState

instance showGameState :: Show GameState where
  show x = genericShow x
