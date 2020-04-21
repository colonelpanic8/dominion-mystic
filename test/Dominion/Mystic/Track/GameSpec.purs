module Dominion.Mystic.Track.GameSpec where

import Control.Monad.State.Class (class MonadState, modify_, gets)
import Control.Monad.State.Trans (runStateT)
import Data.Foldable (foldl)
import Data.Lens as Lens
import Data.Map as Map
import Data.String as String
import Data.String.Pattern as Pattern
import Data.Traversable (traverse_)
import Data.Tuple as Tuple
import Dominion.Mystic.Data as Data
import Dominion.Mystic.Log.Parse as Parse
import Dominion.Mystic.Track.Game as Game
import Prelude
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)

processLines ::
  forall m.
  MonadState Data.GameState m =>
  String ->
  m Unit
processLines input = traverse_ processLine $ String.split (Pattern.Pattern "\n") input

processLine ::
  forall m.
  MonadState Data.GameState m =>
  String ->
  m Unit
processLine line =
  modify_ \gameState ->
    foldl (flip $ Game.updateGameStateAndHistory line) gameState
      $ Parse.getDeckUpdates line

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
        deckSectionEquals "E" Data._play []
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
      it "handles the crazy interaction between shuffling and drawing" do
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
        deckSectionEquals "E" Data._deck []
        deckSectionEquals "E" Data._play []
        deckSectionEquals "E" Data._hand
          [ c 2 "Copper"
          , c 2 "Estate"
          , c 1 "Inventor"
          ]
        deckSectionEquals "E" Data._discard
          [ c 4 "Copper"
          , c 1 "Inventor"
          , c 1 "Silver"
          , c 1 "Goat"
          ]
  where
  it does action =
    Spec.it does $ map Tuple.fst
      $ runStateT action Data.emptyGameState

  deckSectionEquals player sectionLens expected = do
    section <- gets $ Lens.view $ Data.playerDeck (Data.Player player) <<< sectionLens
    section `shouldEqual` (Map.fromFoldable expected)

  c quantity name = Tuple.Tuple (Data.Card name) quantity
