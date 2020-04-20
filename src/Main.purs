module Main where

import Data.Traversable (traverse_)
import Dominion.Mystic.Data as Data
import Dominion.Mystic.Game.Manage (updateGameStateAndHistory)
import Dominion.Mystic.Log.DOM as DOM
import Dominion.Mystic.Log.Parse as Parse
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import Prelude
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  Console.log "Game state"
  document <- HTMLDocument.toDocument <$> (Window.document =<< HTML.window)
  gameState <- Ref.new Data.emptyGameState
  let
    handleNodeElements =
      DOM.handleLogUpdates
        $ \line -> do
            let
              updates = Parse.getDeckUpdates line

              doUpdate update = Ref.modify_ (\state -> updateGameStateAndHistory line update state) gameState
            traverse_ doUpdate updates
            Data.GameState state <- Ref.read gameState
            Console.logShow $ state2.stateByPlayer
  DOM.onLogContainerElement document (handleNodeElements <<< Element.toNode)
