module Main where

import Prelude
import Dominion.Mystic.Log.DOM as DOM
import Dominion.Mystic.Log.Parse as Parse
import Effect (Effect)
import Effect.Console as Console
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  Console.log "Actually logging deck updates"
  document <- HTMLDocument.toDocument <$> (Window.document =<< HTML.window)
  let
    handleNodeElements =
      DOM.handleLogUpdates
        $ \line -> do
            Console.log $ "Deck updates: " <> (show $ Parse.getDeckUpdates line)
            Console.log line
  DOM.onLogContainerElement document (handleNodeElements <<< Element.toNode)
