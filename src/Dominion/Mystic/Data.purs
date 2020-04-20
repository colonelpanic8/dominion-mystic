module Dominion.Mystic.Data where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Lens.Iso as Iso
import Data.Lens.Record as Record
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))

newtype Card
  = Card String

derive instance genericCard :: Generic Card _

derive instance eqCard :: Eq Card

derive instance ordCard :: Ord Card

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

derive instance ordPlayer :: Ord Player

instance showPlayer :: Show Player where
  show x = genericShow x

type CardQuantity
  = Tuple Card Int

type CardList
  = Array CardQuantity

data CardListUpdateType
  = Discards
  | Draws
  | Exiles
  | Gains
  | LooksAt
  | Plays
  | PutsIntoHand
  | Returns
  | Reveals
  | Topdecks
  | Trashes

derive instance genericCardListUpdateType :: Generic CardListUpdateType _

derive instance eqCardListUpdateType :: Eq CardListUpdateType

instance showCardListUpdateType :: Show CardListUpdateType where
  show x = genericShow x

data UpdateInfo
  = Shuffles
  | CardListUpdate { type :: CardListUpdateType, cards :: CardList }

derive instance genericUpdateInfo :: Generic UpdateInfo _

derive instance eqUpdateInfo :: Eq UpdateInfo

instance showUpdateInfo :: Show UpdateInfo where
  show x = genericShow x

data DeckUpdate
  = DeckUpdate Player UpdateInfo

derive instance genericDeckUpdate :: Generic DeckUpdate _

derive instance eqDeckUpdate :: Eq DeckUpdate

instance showDeckUpdate :: Show DeckUpdate where
  show x = genericShow x

mkCardListUpdate :: CardListUpdateType -> Player -> CardList -> DeckUpdate
mkCardListUpdate t player cards =
  DeckUpdate player
    $ CardListUpdate { type: t, cards: cards }

shufflesUpdate :: Player -> DeckUpdate
shufflesUpdate player = DeckUpdate player Shuffles

discardsUpdate :: Player -> CardList -> DeckUpdate
discardsUpdate = mkCardListUpdate Discards

drawsUpdate :: Player -> CardList -> DeckUpdate
drawsUpdate = mkCardListUpdate Draws

exilesUpdate :: Player -> CardList -> DeckUpdate
exilesUpdate = mkCardListUpdate Exiles

gainsUpdate :: Player -> CardList -> DeckUpdate
gainsUpdate = mkCardListUpdate Gains

looksAtUpdate :: Player -> CardList -> DeckUpdate
looksAtUpdate = mkCardListUpdate LooksAt

playsUpdate :: Player -> CardList -> DeckUpdate
playsUpdate = mkCardListUpdate Plays

putsIntoHandUpdate :: Player -> CardList -> DeckUpdate
putsIntoHandUpdate = mkCardListUpdate PutsIntoHand

returnsUpdate :: Player -> CardList -> DeckUpdate
returnsUpdate = mkCardListUpdate Returns

revealsUpdate :: Player -> CardList -> DeckUpdate
revealsUpdate = mkCardListUpdate Reveals

topdecksUpdate :: Player -> CardList -> DeckUpdate
topdecksUpdate = mkCardListUpdate Topdecks

trashesUpdate :: Player -> CardList -> DeckUpdate
trashesUpdate = mkCardListUpdate Trashes

type CountsByCardType
  = Map.Map Card Int

type Deck'
  = { discard :: CountsByCardType
    , deck :: CountsByCardType
    , hand :: CountsByCardType
    , play :: CountsByCardType
    , exile :: CountsByCardType
    , tavern :: CountsByCardType
    , island :: CountsByCardType
    }

data Deck
  = Deck Deck'

derive instance genericDeck :: Generic Deck _

derive instance eqDeck :: Eq Deck

instance showDeck :: Show Deck where
  show x = genericShow x

unpackDeck :: Lens.Lens' Deck Deck'
unpackDeck = Lens.lens' unpack
  where
  unpack (Deck record) = Tuple record Deck

_discard :: forall a r. Lens.Lens' { discard :: a | r } a
_discard = Record.prop (SProxy :: SProxy "discard")

_deck :: forall a r. Lens.Lens' { deck :: a | r } a
_deck = Record.prop (SProxy :: SProxy "deck")

_hand :: forall a r. Lens.Lens' { hand :: a | r } a
_hand = Record.prop (SProxy :: SProxy "hand")

_play :: forall a r. Lens.Lens' { play :: a | r } a
_play = Record.prop (SProxy :: SProxy "play")

_exile :: forall a r. Lens.Lens' { exile :: a | r } a
_exile = Record.prop (SProxy :: SProxy "exile")

_tavern :: forall a r. Lens.Lens' { tavern :: a | r } a
_tavern = Record.prop (SProxy :: SProxy "tavern")

_island :: forall a r. Lens.Lens' { island :: a | r } a
_island = Record.prop (SProxy :: SProxy "island")

emptyDeck :: Deck
emptyDeck =
  Deck
    { discard: Map.empty
    , deck: Map.empty
    , play: Map.empty
    , hand: Map.empty
    , exile: Map.empty
    , tavern: Map.empty
    , island: Map.empty
    }

type PlayerState
  = Deck

type GameState'
  = { stateByPlayer :: Map.Map Player PlayerState
    , history :: List.List (Tuple String DeckUpdate)
    , hasCurrentTurn :: Player
    }

data GameState
  = GameState GameState'

derive instance genericGameState :: Generic GameState _

derive instance eqGameState :: Eq GameState

instance showGameState :: Show GameState where
  show x = genericShow x

unpackGameState :: Lens.Lens' GameState GameState'
unpackGameState = Lens.lens' unpack
  where
  unpack (GameState record) = Tuple record GameState

_stateByPlayer :: forall a r. Lens.Lens' { stateByPlayer :: a | r } a
_stateByPlayer = Record.prop (SProxy :: SProxy "stateByPlayer")

_history :: forall a r. Lens.Lens' { history :: a | r } a
_history = Record.prop (SProxy :: SProxy "history")

emptyGameState :: GameState
emptyGameState =
  GameState
    { stateByPlayer: Map.empty
    , history: List.Nil
    , hasCurrentTurn: Player ""
    }

playerDeck :: Player -> Lens.Lens' GameState Deck'
playerDeck player =
  unpackGameState
    <<< _stateByPlayer
    <<< at player
    <<< (Iso.non emptyDeck)
    <<< unpackDeck

type DeckSectionLens
  = Lens.Lens' Deck' CountsByCardType
