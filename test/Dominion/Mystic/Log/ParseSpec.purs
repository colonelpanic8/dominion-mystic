module Dominion.Mystic.Log.ParseSpec where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Dominion.Mystic.Data as Data
import Dominion.Mystic.Log.Parse as Parse
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (Parser, runParser)

expectSuccessfulParsing ::
  forall o. Show o => Eq o => Parser String o -> String -> o -> Aff Unit
expectSuccessfulParsing parser input expected = (runParser input parser) `shouldEqual` (Right expected)

expectParseLine :: String -> Data.DeckUpdate -> Aff Unit
expectParseLine input expected = expectSuccessfulParsing Parse.parseLine input [ expected ]

c :: Int -> String -> Data.CardQuantity
c quantity name = Tuple (Data.Card name) quantity

u :: Data.CardListUpdateType -> String -> Data.CardList -> Data.DeckUpdate
u t name cards = Data.mkCardListUpdate t (Data.Player name) cards

parseLogSpec :: Spec Unit
parseLogSpec =
  describe "Parsing" do
    let
      shelters =
        [ c 1 "Hovel"
        , c 1 "Necropolis"
        , c 1 "Overgrown Estate"
        ]
    describe "parseLine" do
      it "handles gains" do
        expectParseLine
          "E starts with a Hovel, a Necropolis and an Overgrown Estate."
          $ u Data.Gains "E" shelters
      it "handles trashing" do
        expectParseLine
          "E trashes a Hovel, a Necropolis and an Overgrown Estate."
          $ u Data.Trashes "E" shelters
      it "handles exiles" do
        expectParseLine
          "Edi exiles a Horse"
          $ u Data.Exiles "Edi" [ c 1 "Horse" ]
      it "handles shuffle" do
        expectParseLine
          "K shuffles their deck."
          $ Data.shufflesUpdate
          $ Data.Player "K"
      it "handles topdecks" do
        expectParseLine
          "L topdecks 2 Coppers and a Silver."
          $ u Data.Topdecks "L"
              [ c 2 "Copper"
              , c 1 "Silver"
              ]
      it "handles wishing" do
        expectParseLine
          "E wishes for Ironmonger and finds it."
          $ u Data.Draws "E" [ c 1 "Ironmonger" ]
      it "handles returns" do
        expectParseLine
          "E returns an Estate to the Estate pile."
          $ u Data.Returns "E" [ c 1 "Estate" ]
      it "handles returns with no pile mention" do
        expectParseLine
          "E returns a Horse."
          $ u Data.Returns "E" [ c 1 "Horse" ]
      it "handles looks at" do
        expectParseLine
          "E looks at 3 Estates, 3 Horses, an Ambassador, a Changeling, 5 Coppers and 4 Sleighs."
          $ u Data.LooksAt "E"
              [ c 3 "Estate"
              , c 3 "Horse"
              , c 1 "Ambassador"
              , c 1 "Changeling"
              , c 5 "Copper"
              , c 4 "Sleigh"
              ]
      it "handles plays" do
        expectParseLine
          "L plays a Gold and 3 Coppers."
          $ u Data.Plays "L"
              [ c 1 "Gold"
              , c 3 "Copper"
              ]
      it "handles leading whitespace" do
        expectParseLine
          "   E reveals a Mystic and 2 Provinces."
          $ u Data.Reveals "E"
              [ c 1 "Mystic"
              , c 2 "Province"
              ]
      it "handles reveals" do
        expectParseLine
          "E reveals a Mystic and 2 Provinces."
          $ u Data.Reveals "E"
              [ c 1 "Mystic"
              , c 2 "Province"
              ]
      it "handles buys and gains" do
        expectParseLine
          "L buys and gains a Silver."
          $ u Data.Gains "L"
          $ [ c 1 "Silver" ]
      it "handles turns" do
        expectParseLine
          "Turn 1 - EyeVanMaliceSon"
          $ Data.turnUpdate
          $ Data.Player "EyeVanMaliceSon"
    describe "parseCardsList" do
      it "handles a list of cards separated by commas and an \"and\"" do
        expectSuccessfulParsing Parse.parseCardList
          "a Hovel, a Necropolis and an Overgrown Estate."
          shelters
      it "handles depluralization exceptions" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Nobles, a Hovel."
          [ c 3 "Nobles"
          , c 1 "Hovel"
          ]
      it "handles pluralization exceptions" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Emporia, a Hovel."
          [ c 3 "Emporium"
          , c 1 "Hovel"
          ]
      it "handles quantities of a card other than one and depluralizes" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Coppers, a Hovel and an Overgrown Estate."
          [ c 3 "Copper"
          , c 1 "Hovel"
          , c 1 "Overgrown Estate"
          ]
      it "can handle a list with no terminating period" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Coppers and 2 Horses"
          [ c 3 "Copper"
          , c 2 "Horse"
          ]
