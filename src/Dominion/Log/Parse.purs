module Dominion.Log.Parse where

import Prelude

import Dominion.Data (Card, DeckUpdate)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators as Combinators
import Text.Parsing.Parser.String as String

parseLine :: String -> Array DeckUpdate
parseLine _ = []

parseStartsWith :: Parser String (Array DeckUpdate)
parseStartsWith = do
  pure []

parseCardList :: Parser String (Array Card)
parseCardList = do
  Combinators.optional $ String.string "a "
  Combinators.optional $ String.string "an "
  pure []
