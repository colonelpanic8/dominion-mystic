module Dominion.Mystic.Data where

import Prelude
import Control.Monad.Writer as Writer
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Lens.Iso as Iso
import Data.Lens.Record as Record
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Profunctor.Strong (class Strong)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_, intercalate)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

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
  | Inserts
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
  | Turn
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

turnUpdate :: Player -> DeckUpdate
turnUpdate player = DeckUpdate player Turn

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

insertsUpdate :: Player -> CardList -> DeckUpdate
insertsUpdate = mkCardListUpdate Inserts

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

getCards :: DeckUpdate -> CardList
getCards (DeckUpdate _ (CardListUpdate { cards: cards })) = cards

getCards _ = []

hasAnonymousCards :: DeckUpdate -> Boolean
hasAnonymousCards update =
  Maybe.isJust
    $ find ((_ == Card "card") <<< Tuple.fst)
    $ getCards update

type CountsByCardType
  = Map.Map Card Int

type DeckSection
  = CountsByCardType

type Deck'
  = { discard :: DeckSection
    , deck :: DeckSection
    , hand :: DeckSection
    , play :: DeckSection
    , exile :: DeckSection
    , tavern :: DeckSection
    , island :: DeckSection
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

prettyDeckString :: Deck' -> String
prettyDeckString deck =
  Writer.execWriter do
    prettyDeckSection "Discard" _discard
    prettyDeckSection "Deck" _deck
    prettyDeckSection "Hand" _hand
    prettyDeckSection "Play" _play
  where
  prettyDeckSection sectionName sectionLens = do
    Writer.tell sectionName
    Writer.tell ":"
    Writer.tell "\n"
    let
      cards = Map.toUnfoldable $ Lens.view sectionLens deck
    if List.length cards == 0 then
      Writer.tell " (Nothing)\n"
    else
      traverse_ prettyCardQuantity $ cards

  prettyCardQuantity (Tuple.Tuple (Card name) count) = do
    Writer.tell " - "
    Writer.tell $ show count
    Writer.tell " "
    Writer.tell name
    Writer.tell "\n"

type PlayerState
  = Deck

type GameState'
  = { stateByPlayer :: Map.Map Player PlayerState
    , history :: List.List (Tuple String DeckUpdate)
    , hasCurrentTurn :: Maybe.Maybe Player
    , playersToIgnore :: Set.Set Player
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

_hasCurrentTurn :: forall a r. Lens.Lens' { hasCurrentTurn :: a | r } a
_hasCurrentTurn = Record.prop (SProxy :: SProxy "hasCurrentTurn")

_playersToIgnore :: forall a r. Lens.Lens' { playersToIgnore :: a | r } a
_playersToIgnore = Record.prop (SProxy :: SProxy "playersToIgnore")

knownPlayers :: GameState -> Set.Set Player
knownPlayers = Map.keys <<< Lens.view (unpackGameState <<< _stateByPlayer)

activePlayers :: GameState -> Set.Set Player
activePlayers state =
  Set.difference (knownPlayers state)
    $ Lens.view (unpackGameState <<< _playersToIgnore) state

emptyGameState :: GameState
emptyGameState =
  GameState
    { stateByPlayer: Map.empty
    , history: List.Nil
    , hasCurrentTurn: Maybe.Nothing
    , playersToIgnore: Set.empty
    }

ignorePlayer :: Player -> GameState -> GameState
ignorePlayer player = Lens.over (unpackGameState <<< _playersToIgnore) $ Set.insert player

shouldIgnoreUpdate :: DeckUpdate -> GameState -> Boolean
shouldIgnoreUpdate (DeckUpdate player (CardListUpdate { cards: cards })) = playerIsIgnored player

shouldIgnoreUpdate _ = const false

playerIsIgnored :: Player -> GameState -> Boolean
playerIsIgnored player = Set.member player <<< Lens.view (unpackGameState <<< _playersToIgnore)

prettyGameStateString :: GameState -> String
prettyGameStateString state =
  intercalate "\n" $ map getDeckString
    $ (Set.toUnfoldable $ activePlayers state :: List.List _)
  where
  getDeckString player = prettyDeckString $ Lens.view (playerDeck player) state

playerDeck :: Player -> Lens.Lens' GameState Deck'
playerDeck player =
  unpackGameState
    <<< _stateByPlayer
    <<< at player
    <<< (Iso.non emptyDeck)
    <<< unpackDeck

playerDeckSection ::
  forall t a.
  Strong t =>
  Player ->
  (a -> t Deck' Deck') ->
  a -> t GameState GameState
playerDeckSection player section = playerDeck player <<< section

type DeckSectionLens
  = Lens.Lens' Deck' DeckSection

data GameUpdateError
  = NegativeCardQuantity
    { card :: Card
    , section :: Maybe.Maybe String
    , update :: Maybe.Maybe DeckUpdate
    }

derive instance genericGameUpdateError :: Generic GameUpdateError _

derive instance eqGameUpdateError :: Eq GameUpdateError

instance showGameUpdateError :: Show GameUpdateError where
  show x = genericShow x
