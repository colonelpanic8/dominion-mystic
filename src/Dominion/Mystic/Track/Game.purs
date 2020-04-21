module Dominion.Mystic.Track.Game where

import Data.Array as Array
import Data.Foldable (foldr, class Foldable)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Lens.Iso as Iso
import Data.List as List
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Dominion.Mystic.Data as Data
import Effect.Exception.Unsafe as Unsafe
import Prelude

-- XXX Remove this once things actually work
throwAdd :: Data.Card -> Int -> Int -> Int
throwAdd (Data.Card card) a b =
  if sum < 0 then
    Unsafe.unsafeThrow $ "Got a negative with " <> card <> " left: " <> show a <> " right: " <> show b
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

removeCardsFrom ::
  forall f.
  Foldable f =>
  Functor f =>
  Data.DeckSectionLens ->
  f Data.CardQuantity ->
  Data.Deck' ->
  Data.Deck'
removeCardsFrom section cards deck =
  Lens.over section
    (incrementCards $ map (Lens.over Lens._2 negate) cards)
    deck

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

updateGameStateAndHistory :: String -> Data.DeckUpdate -> Data.GameState -> Data.GameState
updateGameStateAndHistory line update state =
  Lens.over (Data.unpackGameState <<< Data._history)
    (List.Cons $ Tuple line update)
    $ updateGameState
        update
        state

updateGameState :: Data.DeckUpdate -> Data.GameState -> Data.GameState
updateGameState (Data.DeckUpdate player Data.Turn) state = state

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
