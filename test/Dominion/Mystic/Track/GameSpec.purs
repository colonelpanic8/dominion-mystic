module Dominion.Mystic.Track.GameSpec where

import Control.Monad.State.Class (class MonadState, gets, get, put)
import Control.Monad.State.Trans (runStateT)
import Data.Either as Either
import Data.Foldable (foldl)
import Data.Lens as Lens
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Pattern as Pattern
import Data.Traversable (traverse_)
import Data.Tuple as Tuple
import Dominion.Mystic.Data as Data
import Dominion.Mystic.Log.Parse as Parse
import Dominion.Mystic.Track.Game as Game
import Effect.Class.Console as Console
import Effect.Class (class MonadEffect)
import Prelude
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual, fail)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

rightToMaybe :: forall a b. Either.Either a b -> Maybe.Maybe b
rightToMaybe = Either.either (const Maybe.Nothing) Maybe.Just

logState :: Boolean
logState = false

processLines ::
  forall m.
  MonadState Data.GameState m =>
  MonadThrow Error m =>
  MonadEffect m =>
  String ->
  m Unit
processLines input = traverse_ processLine $ String.split (Pattern.Pattern "\n") input

processLine ::
  forall m.
  MonadState Data.GameState m =>
  MonadThrow Error m =>
  MonadEffect m =>
  String ->
  m Unit
processLine line = do
  state <- get
  let
    updates = Parse.getDeckUpdates line

    result =
      foldl (\s update -> s >>= Game.updateGameStateAndHistory line update)
        (Either.Right state)
        updates
  case result of
    (Either.Left err) -> do
      Console.log $ show updates
      Console.log $ show err
      Console.log $ show state
      fail $ "Got error processing: " <> line
    (Either.Right newState) -> do
      put newState
      when logState do
        Console.log line
        Console.log $ Data.prettyGameStateString newState

trackGameSpec :: Spec.Spec Unit
trackGameSpec =
  Spec.describe "Game State Tracking" do
    Spec.describe "updateGameState" do
      it "adds cards at setup" do
        processLines
          """
L starts with a Goat.
L starts with 6 Coppers.
L starts with 3 Estates.
E starts with a Goat.
E starts with 6 Coppers.
E starts with 3 Estates.
"""
        deckSectionEquals "E" Data._discard
          [ c 6 "Copper"
          , c 3 "Estate"
          , c 1 "Goat"
          ]
        processLines
          """
L shuffles their deck.
L draws 5 cards.
E shuffles their deck.
E draws 4 Coppers and an Estate.
"""
        deckSectionEquals "E" Data._hand
          [ c 4 "Copper"
          , c 1 "Estate"
          ]
      it "does cleanup on turn end/start" do
        processLines
          """
L starts with a Goat.
L starts with 6 Coppers.
L starts with 3 Estates.
E starts with a Goat.
E starts with 6 Coppers.
E starts with 3 Estates.
L shuffles their deck.
L draws 5 cards.
E shuffles their deck.
E draws 4 Coppers and an Estate.

Turn 1 - EyeVanMaliceSon
E plays 4 Coppers. (+$4)
E buys and gains an Inventor.
E draws 2 Coppers, 2 Estates and a Goat.

Turn 1 - Lord Rattington
"""
        deckSectionEmpty "E" Data._play
        deckSectionEquals "E" Data._hand
          [ c 2 "Copper"
          , c 2 "Estate"
          , c 1 "Goat"
          ]
        deckSectionEquals "E" Data._discard
          [ c 4 "Copper"
          , c 1 "Estate"
          , c 1 "Inventor"
          ]
      it "handles the interaction between shuffling and drawing" do
        processLines
          """
L starts with a Goat.
L starts with 6 Coppers.
L starts with 3 Estates.
E starts with a Goat.
E starts with 6 Coppers.
E starts with 3 Estates.
L shuffles their deck.
L draws 5 cards.
E shuffles their deck.
E draws 4 Coppers and an Estate.

Turn 1 - EyeVanMaliceSon
E plays 4 Coppers. (+$4)
E buys and gains an Inventor.
E draws 2 Coppers, 2 Estates and a Goat.

Turn 2 - EyeVanMaliceSon
E plays a Goat. (+$1)
E trashes an Estate.
E plays 2 Coppers. (+$2)
E buys and gains a Silver.
E shuffles their deck.
E draws 2 Coppers, 2 Estates and an Inventor.

Turn 1 - Lord Rattington
"""
        deckSectionEmpty "E" Data._play
        deckSectionEquals "E" Data._hand
          [ c 2 "Copper"
          , c 2 "Estate"
          , c 1 "Inventor"
          ]
        deckSectionEquals "E" Data._deck
          [ c 4 "Copper"
          , c 1 "Silver"
          , c 1 "Goat"
          ]
      it "shuffles cards discarded from play even when the deck contained all drawn cards" do
        processLines
          """
E starts with 7 Coppers.
E starts with 3 Estates.
L starts with 7 Coppers.
L starts with 3 Estates.
E shuffles their deck.
E draws 3 Coppers and 2 Estates.
L shuffles their deck.
L draws 5 cards.

Turn 1 - EyeVanMaliceSon
E plays 3 Coppers. (+$3)
E buys and gains a Forager.
E draws 4 Coppers and an Estate.

Turn 1 - Lord Rattington
L plays 3 Coppers. (+$3)
L buys and gains a Forager.
L draws 5 cards.

Turn 2 - EyeVanMaliceSon
E plays 4 Coppers. (+$4)
E buys and gains a Black Market.
E shuffles their deck.
E draws 2 Coppers, an Estate, a Forager and a Black Market.

Turn 2 - Lord Rattington
"""
        deckSectionEmpty "E" Data._play
        processLines
          """
L plays 4 Coppers. (+$4)
L buys and gains a Forager.
L shuffles their deck.
L draws 5 cards.

Turn 3 - EyeVanMaliceSon
E plays a Forager.
E gets +1 Action.
E gets +1 Buy.
E trashes an Estate.
"""
        deckSectionEquals "E" Data._hand
          [ c 2 "Copper"
          , c 1 "Black Market"
          ]
        processLines
          """
E gets 1 VP from Tomb.
E plays a Black Market.
E gets +$2.
E reveals a Goatherd, an Advisor and a Poacher.
E plays 2 Coppers. (+$2)
E gains an Advisor.
E puts a Poacher and a Goatherd on the bottom of the Black Market Deck.
E draws 3 Coppers and 2 Estates.

Turn 3 - Lord Rattington
"""
        deckSectionEmpty "E" Data._play
        deckSectionEquals "E" Data._hand
          [ c 3 "Copper"
          , c 2 "Estate"
          ]
        processLines
          """
L plays 3 Coppers. (+$3)
L buys and gains a Silver.
L draws 5 cards.

Turn 4 - EyeVanMaliceSon
E plays 3 Coppers. (+$3)
E buys and gains a Forager.
E shuffles their deck.
E draws 3 Coppers, a Forager and a Black Market.

Turn 4 - Lord Rattington
"""
        deckSectionEmpty "E" Data._play
        deckSectionEmpty "E" Data._discard
        deckSectionEquals "E" Data._hand
          [ c 3 "Copper"
          , c 1 "Forager"
          , c 1 "Black Market"
          ]
        deckSectionEquals "E" Data._deck
          [ c 4 "Copper"
          , c 1 "Advisor"
          , c 2 "Estate"
          , c 1 "Forager"
          ]
  where
  it does action =
    Spec.it does $ map Tuple.fst
      $ runStateT action Data.emptyGameState

  deckSectionEquals player sectionLens expected = do
    section <- gets $ Lens.view $ Data.playerDeck (Data.Player player) <<< sectionLens
    section `shouldEqual` (Map.fromFoldable expected)

  deckSectionEmpty player sectionLens = deckSectionEquals player sectionLens []

  c quantity name = Tuple.Tuple (Data.Card name) quantity
