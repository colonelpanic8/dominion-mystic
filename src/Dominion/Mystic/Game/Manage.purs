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

incrementCard :: Data.CardQuantity -> Data.CountsByCardType -> Data.CountsByCardType
incrementCard (Tuple card quantity) counts = Lens.over (at card <<< Iso.non 0) (add quantity) counts

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

gainToDiscard ::
  forall f.
  Foldable f =>
  Data.Player ->
  f Data.CardQuantity ->
  Data.GameState ->
  Data.GameState
gainToDiscard player = incrementCardsTo player Data._discard

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
  Lens.over fromSection
    (incrementCards $ map (Lens.over Lens._2 negate) cards)
    newPlaces
  where
  newPlaces = Lens.over toSection (incrementCards cards) deck

updateGameState :: Data.GameState -> Data.DeckUpdate -> Data.GameState
updateGameState state (Data.DeckUpdate player info) = case info of
  Data.Shuffles ->
    let
      discard :: Data.CardList
      discard =
        Map.toUnfoldable
          $ Lens.view (playerDeckSection player Data._discard) state

      shuffled = incrementCardsTo player Data._deck discard state
    in
      setDeckSection player Data._discard Map.empty shuffled
  Data.CardListUpdate { type: Data.Gains, cards: cards } -> gainToDiscard player cards state
  -- (Data.Draws player cards) -> move player cards Data._deck Data._hand
  -- (Data.Exiles player cards) -> move player cards Data._hand Data._exile
  _ -> state
