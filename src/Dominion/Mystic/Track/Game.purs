module Dominion.Mystic.Track.Game where

import Prelude
import Data.Either as Either
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
  Either.note (Data.NegativeCardQuantity { card: card, section: Maybe.Nothing, update: Maybe.Nothing })
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

doTurnCleanup :: Data.Deck' -> DeckUpdate
doTurnCleanup =
  transferAllCards Data._hand Data._discard
    >=> transferAllCards Data._play Data._discard

shuffleDeck :: Data.Deck' -> DeckUpdate
shuffleDeck = transferAllCards Data._discard Data._deck

updateGameStateAndHistory ::
  String ->
  Data.DeckUpdate ->
  Data.GameState ->
  GameStateUpdate
updateGameStateAndHistory line update@(Data.DeckUpdate updatePlayer _) state
  | Data.shouldIgnoreUpdate update state = Either.Right state
  | Data.hasAnonymousCards update = Either.Right $ Data.ignorePlayer updatePlayer state
  | otherwise =
    Lens.over (Data.unpackGameState <<< Data._history)
      (List.Cons $ Tuple line update)
      <$> (tryCleanup currentPlayer $ updateGameState update state)
    where
    currentPlayer = Lens.view (Data.unpackGameState <<< Data._hasCurrentTurn) state

    tryCleanup (Maybe.Just player) ( Either.Left
        (Data.NegativeCardQuantity _)
    ) =
      overJust (Data.playerDeck player)
        doTurnCleanup
        state
        >>= updateGameState (Data.DeckUpdate player Data.Shuffles)
        >>= updateGameState update

    tryCleanup _ r = r

updateGameState :: Data.DeckUpdate -> Data.GameState -> GameStateUpdate
updateGameState ( Data.DeckUpdate
    (Data.Player turnPlayer)
    Data.Turn
) state@(Data.GameState state') =
  updateCurrentTurn
    <$> ( Maybe.maybe (Either.Right state)
          cleanupState
          state'.hasCurrentTurn
      )
  where
  -- TODO: Remove the need to do this lookup
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
        ( Data.DeckUpdate
            dPlayer
            (Data.CardListUpdate { type: Data.Draws })
        ) -> dPlayer == cleanupTurnPlayer
        _ -> false

      didDrawTriggerShuffle :: List.List (Tuple String Data.DeckUpdate) -> Boolean
      didDrawTriggerShuffle ( List.Cons
          ( Tuple.Tuple
            _
            ( Data.DeckUpdate
              shufflePlayer
              Data.Shuffles
          )
        )
          _
      ) = shufflePlayer == cleanupTurnPlayer

      didDrawTriggerShuffle _ = false

      rewindToDraw :: List.List (Tuple String Data.DeckUpdate) -> Tuple.Tuple Data.CardList Boolean
      rewindToDraw (List.Cons (Tuple.Tuple _ draw) remaining) =
        if isTurnDraw draw then
          Tuple.Tuple (Data.getCards draw) (didDrawTriggerShuffle remaining)
        else
          rewindToDraw remaining

      -- XXX: This case should never be reached. Error here?
      rewindToDraw _ = Tuple.Tuple [] false

      Tuple.Tuple lastCardsDrawn drawTriggeredShuffle =
        rewindToDraw
          $ Lens.view (Data.unpackGameState <<< Data._history) state

      cleanupAction =
        if drawTriggeredShuffle then
          doTurnCleanup >=> shuffleDeck
        else
          doTurnCleanup

      cleanupTransformation =
        removeCardsFrom Data._hand lastCardsDrawn
          >=> cleanupAction
          >=> addCardsTo Data._hand lastCardsDrawn
    in
      overJust (Data.playerDeck cleanupTurnPlayer) cleanupTransformation state

updateGameState (Data.DeckUpdate player Data.Shuffles) state =
  overJust (Data.playerDeck player) shuffleDeck
    state

updateGameState ( Data.DeckUpdate
    player
    (Data.CardListUpdate { type: t, cards: cards })
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
