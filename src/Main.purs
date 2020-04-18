module Main where

import Prelude

import Data.Maybe as Maybe
import Dominion.Log.DOM as DOM
import Effect (Effect)
import Effect.Console (log)
import Web.HTML (window)
import Web.DOM.Element as Element
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  document <- toDocument <$> (document =<< window)
  DOM.onLogContainerElement document ((DOM.handleLogUpdates log) <<< Element.toNode)
  elem <- DOM.getLogContainerElement document
  log $ show $ Maybe.isJust elem
  Maybe.maybe (pure unit) (DOM.handleLogUpdates log) (Element.toNode <$> elem)
