module Dominion.Mystic.Track.Game where

import Prelude
import Data.Array as Array
import Data.Foldable (foldr, class Foldable, find)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Lens.Iso as Iso
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Dominion.Mystic.Data as Data
import Effect.Exception.Unsafe as Unsafe

-- XXX Remove this once things actually work
throwAdd :: Data.Card -> Int -> Int -> Int
throwAdd (Data.Card card) a b =
  if sum < 0 then
    Unsafe.unsafeThrow
      $ "Got a negative with "
      <> card
      <> " left: "
      <> show a
      <> " right: "
      <> show b
  else
    sum
  where
  sum = a + b

incrementCard ::
  Data.CardQuantity ->
  Data.CountsByCardType ->
  Data.CountsByCardType
incrementCard (Tuple card quantity) counts =
  Lens.over (at card <<< Iso.non 0) (throwAdd card quantity)
    counts

incrementCards ::
  forall f.
  Foldable f =>
  f Data.CardQuantity ->
  Data.CountsByCardType ->
  Data.CountsByCardType
incrementCards = flip $ foldr incrementCard

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
  Data.Deck'
addCardsTo section cards = Lens.over section $ incrementCards cards

removeCardsFrom ::
  forall f.
  Foldable f =>
  Functor f =>
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.Deck' ->
  Data.Deck'
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
  Data.GameState
incrementCardsTo player deckSection cardQuantities =
  Lens.over (Data.playerDeckSection player deckSection)
    (incrementCards cardQuantities)

transferCards ::
  forall f.
  Foldable f =>
  Functor f =>
  Data.DeckSectionLens ->
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.Deck' ->
  Data.Deck'
transferCards fromSection toSection cards deck =
  removeCardsFrom fromSection cards
    $ Lens.over toSection (incrementCards cards) deck

transferAllCards ::
  Data.DeckSectionLens ->
  Data.DeckSectionLens ->
  Data.Deck' ->
  Data.Deck'
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
  Data.GameState
updateGameStateAndHistory line update state =
  Lens.over (Data.unpackGameState <<< Data._history)
    (List.Cons $ Tuple line update)
    $ updateGameState
        update
        state

endTurn :: Data.Deck' -> Data.Deck'
endTurn =
  transferAllCards Data._play Data._discard
    <<< transferAllCards Data._hand Data._discard

updateGameState :: Data.DeckUpdate -> Data.GameState -> Data.GameState
updateGameState ( Data.DeckUpdate
    (Data.Player turnPlayer)
    Data.Turn
) state@(Data.GameState state') =
  updateCurrentTurn
    $ Maybe.fromMaybe state
    $ cleanupState
    <$> state'.hasCurrentTurn
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
        _ -> []

      cleanupTransformation =
        addCardsTo Data._hand lastCardsDrawn <<< endTurn
          <<< removeCardsFrom Data._hand lastCardsDrawn
    in
      Lens.over (Data.playerDeck cleanupTurnPlayer) cleanupTransformation state

updateGameState (Data.DeckUpdate player Data.Shuffles) state =
  let
    discard :: Data.CardList
    discard =
      Map.toUnfoldable
        $ Lens.view (Data.playerDeckSection player Data._discard) state

    shuffled = incrementCardsTo player Data._deck discard state
  in
    setDeckSection player Data._discard Map.empty shuffled

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
      _ -> state
  )
  where
  cards = Array.filter ((_ /= Data.Card "card") <<< Tuple.fst) cards'

  overPlayerDeck = Lens.over $ Data.playerDeck player

  overState action = overPlayerDeck action state

  move ::
    forall f.
    Foldable f =>
    Functor f =>
    f Data.CardQuantity ->
    Data.DeckSectionLens ->
    Data.DeckSectionLens ->
    Data.GameState
  move c source dest = overState $ transferCards source dest c

  mv :: Data.DeckSectionLens -> Data.DeckSectionLens -> Data.GameState
  mv = move cards

  gainTo :: Data.DeckSectionLens -> Data.GameState
  gainTo lens = incrementCardsTo player lens cards state

  removeFromHand :: Unit -> Data.GameState
  removeFromHand _ = overState $ removeCardsFrom Data._hand cards
