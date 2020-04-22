module Main where

import Data.Either as Either
import Data.Traversable (traverse_)
import Dominion.Mystic.Data as Data
import Dominion.Mystic.Log.DOM as DOM
import Dominion.Mystic.Log.Parse as Parse
import Dominion.Mystic.Track.Game (updateGameStateAndHistory)
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

              doUpdate update = do
                state <- Ref.read gameState
                case updateGameStateAndHistory line update state of
                  Either.Left err -> Console.logShow err
                  Either.Right updated -> Ref.write updated gameState
            traverse_ doUpdate updates
            Data.GameState state <- Ref.read gameState
            Console.logShow $ state.stateByPlayer
  DOM.onLogContainerElement document (handleNodeElements <<< Element.toNode)
