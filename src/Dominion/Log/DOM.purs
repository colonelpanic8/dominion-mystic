module Dominion.Log.DOM where

import Data.Array (head, concat, catMaybes)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Prelude
import Web.DOM (Document, Element, Node)
import Web.DOM.DOMTokenList (contains)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.MutationRecord (MutationRecord, addedNodes)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList

getLogContainerElement :: Document -> Effect (Maybe Element)
getLogContainerElement document = do
  head <$> (Document.getElementsByClassName "game-log" document >>= HTMLCollection.toArray)

onLogContainerElement :: Document -> (Element -> Effect Unit) -> Effect Unit
onLogContainerElement document callback = do
  observer <- MutationObserver.mutationObserver
              (\records observer -> do
                  (traverse_ callback <<< catMaybes) =<<
                    traverse getLogContainerFromRecord records
                  e <- getLogContainerElement document
                  case e of
                    Just elem -> do
                      MutationObserver.disconnect observer
                      callback elem
                    Nothing -> pure unit)
  MutationObserver.observe (Document.toNode document) { childList: true, subtree: true } observer

getLogContainerFromRecord :: MutationRecord -> Effect (Maybe Element)
getLogContainerFromRecord record = do
  elements <- (catMaybes <<< (map Element.fromNode)) <$>
              (addedNodes record >>= NodeList.toArray)
  foldM doFind Nothing elements
    where doFind Nothing element =
            do
              found <- elementIsLogContainer element
              pure if found
                then Just element
                else Nothing
          doFind result _ = pure result

elementIsLogContainer :: Element -> Effect Boolean
elementIsLogContainer element = do
  classes <- Element.classList element
  contains classes "game-log"

handleLogUpdates :: (String -> Effect Unit) -> Node -> Effect Unit
handleLogUpdates callback logContainerNode = do
  observer <- MutationObserver.mutationObserver
              (\records _ ->
                getLinesFromRecords records >>= traverse_ callback)
  MutationObserver.observe logContainerNode { childList: true, subtree: true } observer

getLinesFromRecords :: Array MutationRecord -> Effect (Array String)
getLinesFromRecords records = concat <$> (traverse getLinesFromRecord records)

getLinesFromRecord :: MutationRecord -> Effect (Array String)
getLinesFromRecord record = do
  addedNodes record >>= NodeList.toArray >>= traverse Node.textContent
