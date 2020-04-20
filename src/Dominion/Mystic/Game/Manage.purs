module Dominion.Mystic.Game.Manage where

import Prelude
import Data.Foldable (foldr, class Foldable)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Lens.Iso as Iso
import Data.Map as Map
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Dominion.Mystic.Data as Data

incrementCard ::
  Data.CardQuantity ->
  Data.CountsByCardType ->
  Data.CountsByCardType
incrementCard (Tuple card quantity) counts =
  Lens.over (at card <<< Iso.non 0) (add quantity)
    counts

incrementCards ::
  forall f.
  Foldable f =>
  f Data.CardQuantity ->
  Data.CountsByCardType ->
  Data.CountsByCardType
incrementCards = flip $ foldr incrementCard

playerDeckSection ::
  forall t a.
  Strong t =>
  Data.Player ->
  (a -> t Data.Deck' Data.Deck') ->
  a -> t Data.GameState Data.GameState
playerDeckSection player section = Data.playerDeck player <<< section

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
setDeckSection player = Lens.set <<< playerDeckSection player

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
  Lens.over (playerDeckSection player deckSection)
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

updateGameState :: Data.GameState -> Data.DeckUpdate -> Data.GameState
updateGameState state (Data.DeckUpdate player Data.Shuffles) =
  let
    discard :: Data.CardList
    discard =
      Map.toUnfoldable
        $ Lens.view (playerDeckSection player Data._discard) state

    shuffled = incrementCardsTo player Data._deck discard state
  in
    setDeckSection player Data._discard Map.empty shuffled

updateGameState state ( Data.DeckUpdate
    player
    (Data.CardListUpdate { type: t, cards: cards })
) =
  ( case t of
      Data.Discards -> mv Data._hand Data._discard
      Data.Draws -> mv Data._deck Data._hand
      Data.Exiles -> mv Data._hand Data._exile
      Data.Gains -> gainToDiscard
      Data.Plays -> mv Data._hand Data._play
      Data.PutsIntoHand -> mv Data._deck Data._hand
      Data.Returns -> removeFromHand
      Data.Topdecks -> mv Data._hand Data._deck
      Data.Trashes -> removeFromHand
      _ -> state
  )
  where
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
  move c source dest =
    Lens.over (Data.playerDeck player)
      (transferCards source dest c)
      state

  mv :: Data.DeckSectionLens -> Data.DeckSectionLens -> Data.GameState
  mv = move cards

  gainTo :: Data.DeckSectionLens -> Data.GameState
  gainTo lens = incrementCardsTo player lens cards state

  gainToDiscard = gainTo Data._discard

  gainToHand = gainTo Data._hand

  removeFromHand = overState $ removeCardsFrom Data._hand cards
