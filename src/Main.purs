module Main where

import Prelude

import Data.Maybe as Maybe
import Dominion.Log.DOM as DOM
import Dominion.Log.Parse as Parse
import Effect (Effect)
import Effect.Console as Console
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Debug.Trace as Trace

main :: Effect Unit
main = do
  Console.log "Logging output of empty parseLine"
  document <- HTMLDocument.toDocument <$> (Window.document =<< HTML.window)
  DOM.onLogContainerElement document ((DOM.handleLogUpdates Console.log) <<< Element.toNode)
  elem <- DOM.getLogContainerElement document
  Maybe.maybe (pure unit)
    (DOM.handleLogUpdates $ \line ->
      do
        Console.log line
        Trace.traceM $ Parse.getDeckUpdates line)
    (Element.toNode <$> elem)
