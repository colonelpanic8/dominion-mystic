module Dominion.Mystic.Track.Game where

import Prelude
import Data.Array as Array
import Data.Either as Either
import Data.Foldable (foldr, class Foldable, find)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Lens.Iso as Iso
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Profunctor.Choice (left)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Dominion.Mystic.Data as Data

type GameStateUpdate
  = Either.Either Data.GameUpdateError Data.GameState

type DeckUpdate
  = Either.Either Data.GameUpdateError Data.Deck'

overJust ::
  forall a b f.
  Functor f =>
  Lens.Lens' a b ->
  (b -> f b) ->
  a ->
  f a
overJust lens fn v = flip (Lens.set lens) v <$> (fn $ Lens.view lens v)

nonNegativeAdd :: Int -> Int -> Maybe.Maybe Int
nonNegativeAdd a b =
  if sum < 0 then
    Maybe.Nothing
  else
    Maybe.Just sum
  where
  sum = a + b

tryIncrementCard ::
  Data.CardQuantity ->
  Data.CountsByCardType ->
  Data.CountsByCardType
tryIncrementCard (Tuple card quantity) counts =
  Lens.over
    (at card <<< Iso.non 0)
    (Maybe.fromMaybe <*> nonNegativeAdd quantity)
    counts

incrementCard ::
  Data.CardQuantity ->
  Data.CountsByCardType ->
  Either.Either Data.GameUpdateError Data.CountsByCardType
incrementCard (Tuple card quantity) =
  Either.note (Data.NegativeCardQuantity card Maybe.Nothing)
    <<< overJust (at card <<< Iso.non 0) (nonNegativeAdd quantity)

incrementCards ::
  forall f.
  Foldable f =>
  f Data.CardQuantity ->
  Data.CountsByCardType ->
  Either.Either Data.GameUpdateError Data.CountsByCardType
incrementCards cards counts =
  foldr (\cq ds -> ds >>= incrementCard cq) (Either.Right counts)
    cards

setDeckSection ::
  forall a b.
  Data.Player ->
  ( (b -> a) ->
    Data.Deck' ->
    Data.Deck'
  ) ->
  a ->
  Data.GameState ->
  Data.GameState
setDeckSection player = Lens.set <<< Data.playerDeckSection player

addCardsTo ::
  forall f.
  Foldable f =>
  Functor f =>
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.Deck' ->
  DeckUpdate
addCardsTo section cards = overJust section $ incrementCards cards

removeCardsFrom ::
  forall f.
  Foldable f =>
  Functor f =>
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.Deck' ->
  DeckUpdate
removeCardsFrom section cards =
  addCardsTo section
    $ map (Lens.over Lens._2 negate) cards

incrementCardsTo ::
  forall f.
  Foldable f =>
  Data.Player ->
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.GameState ->
  GameStateUpdate
incrementCardsTo player deckSection cardQuantities =
  overJust (Data.playerDeckSection player deckSection)
    (incrementCards cardQuantities)

transferCards ::
  forall f.
  Foldable f =>
  Functor f =>
  Data.DeckSectionLens ->
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.Deck' ->
  DeckUpdate
transferCards fromSection toSection cards deck =
  addCardsTo toSection cards deck
    >>= removeCardsFrom fromSection cards

transferAllCards ::
  Data.DeckSectionLens ->
  Data.DeckSectionLens ->
  Data.Deck' ->
  DeckUpdate
transferAllCards fromSection toSection deck =
  transferCards fromSection toSection cards
    deck
  where
  cards :: Array Data.CardQuantity
  cards = Map.toUnfoldable $ Lens.view fromSection deck

updateGameStateAndHistory ::
  String ->
  Data.DeckUpdate ->
  Data.GameState ->
  GameStateUpdate
updateGameStateAndHistory line update state =
  Lens.over (Data.unpackGameState <<< Data._history)
    (List.Cons $ Tuple line update)
    $ updateGameState
        update
        state


doTurnCleanup :: Data.Deck' -> DeckUpdate
doTurnCleanup =
  transferAllCards Data._hand Data._discard
    >=> transferAllCards Data._play Data._discard

updateGameState :: Data.DeckUpdate -> Data.GameState -> GameStateUpdate
updateGameState ( Data.DeckUpdate
    (Data.Player turnPlayer)
    Data.Turn
) state@(Data.GameState state') =
  updateCurrentTurn
    <$> ( Maybe.fromMaybe (Either.Right state)
          $ cleanupState
          <$> state'.hasCurrentTurn
      )
  where
  newTurnPlayer =
    Maybe.fromMaybe (Data.Player turnPlayer)
      $ find (\(Data.Player p) -> String.take (String.length p) turnPlayer == p)
      $ Map.keys state'.stateByPlayer

  updateCurrentTurn =
    Lens.set (Data.unpackGameState <<< Data._hasCurrentTurn)
      (Maybe.Just newTurnPlayer)

  cleanupState cleanupTurnPlayer =
    let
      isTurnDraw i = case i of
        ( Tuple.Tuple
            _
            (Data.DeckUpdate dPlayer (Data.CardListUpdate { type: Data.Draws }))
        ) -> dPlayer == cleanupTurnPlayer
        _ -> false

      lastCardsDrawn = case find isTurnDraw state'.history of
        Maybe.Just
          ( Tuple.Tuple
            _
            (Data.DeckUpdate _ (Data.CardListUpdate { cards: cards }))
        ) -> cards
        _ -> [] -- TODO: Error here?

      cleanupTransformation =
        removeCardsFrom Data._hand lastCardsDrawn
          >=> doTurnCleanup
          >=> addCardsTo Data._hand lastCardsDrawn
    in
      overJust (Data.playerDeck cleanupTurnPlayer) cleanupTransformation state

updateGameState (Data.DeckUpdate player Data.Shuffles) state =
  let
    discard :: Data.CardList
    discard =
      Map.toUnfoldable
        $ Lens.view (Data.playerDeckSection player Data._discard) state

    shuffled = incrementCardsTo player Data._deck discard state
  in
    setDeckSection player Data._discard Map.empty <$> shuffled

updateGameState ( Data.DeckUpdate
    player
    (Data.CardListUpdate { type: t, cards: cards' })
) state =
  ( case t of
      Data.Discards -> mv Data._hand Data._discard
      Data.Draws -> mv Data._deck Data._hand
      Data.Exiles -> mv Data._hand Data._exile
      Data.Gains -> gainTo Data._discard
      Data.Plays -> mv Data._hand Data._play
      Data.PutsIntoHand -> mv Data._deck Data._hand
      Data.Returns -> removeFromHand unit
      Data.Topdecks -> mv Data._hand Data._deck
      Data.Trashes -> removeFromHand unit
      _ -> Either.Right state
  )
  where
  cards = Array.filter ((_ /= Data.Card "card") <<< Tuple.fst) cards'

  overState :: forall f. Functor f => (Data.Deck' -> f Data.Deck') -> f Data.GameState
  overState action = overJust (Data.playerDeck player) action state

  move ::
    forall f.
    Foldable f =>
    Functor f =>
    f Data.CardQuantity ->
    Data.DeckSectionLens ->
    Data.DeckSectionLens ->
    GameStateUpdate
  move c source dest = overState $ transferCards source dest c

  mv :: Data.DeckSectionLens -> Data.DeckSectionLens -> GameStateUpdate
  mv = move cards

  gainTo :: Data.DeckSectionLens -> GameStateUpdate
  gainTo lens = incrementCardsTo player lens cards state

  removeFromHand :: Unit -> GameStateUpdate
  removeFromHand _ = overState $ removeCardsFrom Data._hand cards
