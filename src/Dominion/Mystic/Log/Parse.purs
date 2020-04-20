module Dominion.Mystic.Log.Parse where

import Prelude
import Data.Array as Array
import Data.Either (either)
import Data.Formatter.Parser.Number (parseInteger)
import Data.List as List
import Data.String.CodeUnits as SCU
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
parseLine = do
  String.skipSpaces
  Combinators.choice
    [ Combinators.try parseShuffle
    , Combinators.try parseWish
    , parseCardListAction Data.discardsUpdate "discards"
    , parseCardListAction Data.drawsUpdate "draws"
    , parseCardListAction Data.exilesUpdate "exiles"
    , parseCardListAction Data.gainsUpdate "gains"
    , parseCardListAction Data.gainsUpdate "starts with"
    , parseCardListAction Data.gainsUpdate "buys and gains"
    , parseCardListAction Data.looksAtUpdate "looks at"
    , parseCardListAction Data.playsUpdate "plays"
    , parseCardListAction Data.returnsUpdate "returns"
    , parseCardListAction Data.revealsUpdate "reveals"
    , parseCardListAction Data.topdecksUpdate "topdecks"
    , parseCardListAction Data.trashesUpdate "trashes"
    ]

parsePlayer :: Parser String Data.Player
parsePlayer = Data.Player <$> SCU.fromCharArray <$> Array.many alphaNum

parseWish :: Parser String (Array Data.DeckUpdate)
parseWish = do
  player <- parsePlayer
  String.skipSpaces
  _ <- String.string "wishes for"
  String.skipSpaces
  cardName <- parseStringTill $ String.string " and finds it."
  pure $ pure $ Data.drawsUpdate player [ Tuple.Tuple (Data.Card cardName) 1 ]

parseShuffle :: Parser String (Array Data.DeckUpdate)
parseShuffle = do
  player <- parsePlayer
  String.skipSpaces
  _ <- String.string "shuffles their deck"
  pure $ pure $ Data.shufflesUpdate player

parseCardListAction ::
  (Data.Player -> Data.CardList -> Data.DeckUpdate) ->
  String ->
  Parser String (Array Data.DeckUpdate)
parseCardListAction constructor actionString = parseCardListActionWithSuffix constructor actionString ""

parseCardListActionWithSuffix ::
  (Data.Player -> Data.CardList -> Data.DeckUpdate) ->
  String ->
  String ->
  Parser String (Array Data.DeckUpdate)
parseCardListActionWithSuffix constructor actionString suffixString =
  pure
    <$> Combinators.try do
        player <- parsePlayer
        String.skipSpaces
        _ <- String.string actionString
        String.skipSpaces
        cardList <- parseCardList
        _ <- String.string suffixString
        pure $ constructor player cardList

cardSplitMatcher :: Parser String String
cardSplitMatcher =
  Combinators.choice
    [ String.string ", "
    , String.string "."
    , String.string " and "
    , String.string " to " -- Hack for e.g. "E returns an Estate to the Estate pile."
    , String.eof *> pure ""
    ]

parseCardList :: Parser String Data.CardList
parseCardList = map dePluralize <$> Array.many parseCardNameQuantity

dePluralize :: Tuple.Tuple Int String -> Data.CardQuantity
dePluralize info = Tuple.Tuple card quantity
  where
  quantity = Tuple.fst info

  card =
    if quantity > 1 then
      Data.cardFromPluralizedName $ Tuple.snd info
    else
      Data.Card $ Tuple.snd info

parseCardNameQuantity :: Parser String (Tuple.Tuple Int String)
parseCardNameQuantity = do
  quantity <-
    Combinators.choice
      [ (String.string "a " *> pure 1)
      , (String.string "an " *> pure 1)
      , parseInteger
      ]
  String.skipSpaces
  cardName <- parseStringTill cardSplitMatcher
  pure $ Tuple.Tuple quantity cardName

parseStringTill :: Parser String String -> Parser String String
parseStringTill stop =
  SCU.fromCharArray <<< List.toUnfoldable
    <$> Combinators.manyTill String.anyChar stop
