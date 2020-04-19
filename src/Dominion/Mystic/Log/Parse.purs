module Dominion.Mystic.Log.Parse where

import Prelude

import Data.Either (either)
import Data.Array as Array
import Data.Formatter.Parser.Number (parseInteger)
import Data.List as List
import Data.Maybe as M
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple as Tuple
import Dominion.Mystic.Data as Data
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser as Parser
import Text.Parsing.Parser.Combinators as Combinators
import Text.Parsing.Parser.String as String
import Text.Parsing.Parser.Token (alphaNum)

getDeckUpdates :: String -> Array Data.DeckUpdate
getDeckUpdates input = either (const []) identity $ Parser.runParser input parseLine

parseLine :: Parser String (Array Data.DeckUpdate)
parseLine = Combinators.choice
            [ cardListDeckUpdateBuilder Data.Gains "starts with"
            , cardListDeckUpdateBuilder Data.Gains "gains"
            , cardListDeckUpdateBuilder Data.Exiles "exiles"
            , cardListDeckUpdateBuilder Data.Trashes "trashes"
            , cardListDeckUpdateBuilder Data.Discards "discards"
            , cardListDeckUpdateBuilder Data.Draws "draws"
            ]

parsePlayer :: Parser String String
parsePlayer = SCU.fromCharArray <$> Array.many alphaNum

cardListDeckUpdateBuilder ::
  (Data.Player -> Data.CardList -> Data.DeckUpdate) -> String -> Parser String (Array Data.DeckUpdate)
cardListDeckUpdateBuilder constructor actionString = pure <$> Combinators.try do
  player <- Data.Player <$> parsePlayer
  String.skipSpaces
  _ <- String.string actionString
  String.skipSpaces
  cardList <- parseCardList
  pure $ constructor player cardList

cardSplitMatcher :: Parser String String
cardSplitMatcher =
  Combinators.choice
  [ String.string ", "
  , String.string "."
  , String.string " and "
  , String.eof *> pure ""
  ]

parseCardList :: Parser String Data.CardList
parseCardList = map dePluralize <$> Array.many parseCardNameQuantity

dePluralize :: Tuple.Tuple Int String -> Data.CardQuantity
dePluralize info = Tuple.Tuple quantity (Data.Card $ M.fromMaybe (Tuple.snd info) dePluralized)
  where quantity = Tuple.fst info
        dePluralized = if quantity > 1
                       then S.stripSuffix (Pattern "s") $ Tuple.snd info
                       else M.Nothing

parseCardNameQuantity :: Parser String (Tuple.Tuple Int String)
parseCardNameQuantity = do
  quantity <- Combinators.choice
              [ (String.string "a " *> pure 1)
              , (String.string "an " *> pure 1)
              , parseInteger
              ]
  String.skipSpaces
  letters <- Combinators.manyTill String.anyChar cardSplitMatcher
  pure $ Tuple.Tuple quantity (SCU.fromCharArray $ List.toUnfoldable letters)
