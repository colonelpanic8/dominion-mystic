module Dominion.Mystic.Log.Parse where

import Prelude
import Data.Array as Array
import Data.Either (either)
import Data.Formatter.Parser.Number (parseInteger)
import Data.List as List
import Data.Maybe as Maybe
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
  flip Combinators.withErrorMessage "Could not parse line"
    $ Combinators.choice
        [ Combinators.try parseShuffle
        , Combinators.try parseTurn
        , Combinators.try parseWish
        , parseCardListAction Data.discardsUpdate "discards"
        , parseCardListAction Data.drawsUpdate "draws"
        , parseCardListAction Data.exilesUpdate "exiles"
        , parseCardListAction Data.gainsUpdate "buys and gains"
        , parseCardListAction Data.gainsUpdate "gains"
        , parseCardListAction Data.gainsUpdate "starts with"
        , parseCardListAction Data.looksAtUpdate "looks at"
        , parseCardListAction Data.playsUpdate "plays"
        , parseCardListAction Data.revealsUpdate "reveals"
        , parseCardListAction Data.topdecksUpdate "topdecks"
        , parseCardListAction Data.trashesUpdate "trashes"
        , parseCardListActionWithSuffix Data.insertsUpdate "inserts" $ Maybe.Just " into their deck"
        , parseCardListActionWithSuffix Data.returnsUpdate "returns" $ Maybe.Just " to the"
        ]

parsePlayer :: Parser String Data.Player
parsePlayer = Data.Player <$> SCU.fromCharArray <$> Array.many alphaNum

parseTurn :: Parser String (Array Data.DeckUpdate)
parseTurn = do
  skipString "Turn"
  String.skipSpaces
  _ <- parseInteger
  String.skipSpaces
  skipString "-"
  String.skipSpaces
  player <- parsePlayer
  pure $ pure $ Data.turnUpdate player

parseShuffle :: Parser String (Array Data.DeckUpdate)
parseShuffle = do
  player <- parsePlayer
  String.skipSpaces
  skipString "shuffles their deck"
  pure $ pure $ Data.shufflesUpdate player

parseCardListAction ::
  (Data.Player -> Data.CardList -> Data.DeckUpdate) ->
  String ->
  Parser String (Array Data.DeckUpdate)
parseCardListAction constructor actionString =
  parseCardListActionWithSuffix
    constructor
    actionString
    Maybe.Nothing

parseCardListActionWithSuffix ::
  (Data.Player -> Data.CardList -> Data.DeckUpdate) ->
  String ->
  Maybe.Maybe String ->
  Parser String (Array Data.DeckUpdate)
parseCardListActionWithSuffix constructor actionString suffixString =
  pure
    <$> Combinators.try do
        player <- parsePlayer
        String.skipSpaces
        skipString actionString
        String.skipSpaces
        cardList <- parseCardList $ Array.fromFoldable $ map skipString suffixString
        pure $ constructor player cardList

cardSplitMatcher :: Array (Parser String Unit) -> Parser String Unit
cardSplitMatcher additional =
  Combinators.choice
    $ [ skipString ", "
      , skipString "."
      , skipString " and "
      , String.eof
      ]
    <> additional

parseCardList :: Array (Parser String Unit) -> Parser String Data.CardList
parseCardList additionalSplitMatchers =
  map dePluralize
    <$> Array.many
        (parseCardNameQuantity additionalSplitMatchers)

dePluralize :: Tuple.Tuple Int String -> Data.CardQuantity
dePluralize info = Tuple.Tuple card quantity
  where
  quantity = Tuple.fst info

  card =
    if quantity > 1 then
      Data.cardFromPluralizedName $ Tuple.snd info
    else
      Data.Card $ Tuple.snd info

parseCardNameQuantity :: Array (Parser String Unit) -> Parser String (Tuple.Tuple Int String)
parseCardNameQuantity additionalSplitMatchers = do
  quantity <-
    Combinators.choice
      [ (String.string "a " *> pure 1)
      , (String.string "an " *> pure 1)
      , parseInteger
      ]
  String.skipSpaces
  cardName <- parseStringTill $ cardSplitMatcher additionalSplitMatchers
  pure $ Tuple.Tuple quantity cardName

parseStringTill :: forall a. Parser String a -> Parser String String
parseStringTill stop =
  SCU.fromCharArray <<< List.toUnfoldable
    <$> Combinators.manyTill String.anyChar stop

parseWish :: Parser String (Array Data.DeckUpdate)
parseWish = do
  player <- parsePlayer
  String.skipSpaces
  skipString "wishes for"
  String.skipSpaces
  cardName <- parseStringTill $ String.string " and finds it."
  pure $ pure $ Data.drawsUpdate player [ Tuple.Tuple (Data.Card cardName) 1 ]

skipString :: String -> Parser String Unit
skipString = void <<< String.string
