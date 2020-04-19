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

parseSpec :: Spec Unit
parseSpec =
  describe "Parsing" do
    let
      shelters =
        [ Tuple 1 $ Data.Card "Hovel"
        , Tuple 1 $ Data.Card "Necropolis"
        , Tuple 1 $ Data.Card "Overgrown Estate"
        ]
    describe "parseLine" do
      it "handles gains" do
        expectParseLine
          "E starts with a Hovel, a Necropolis and an Overgrown Estate."
          $ Data.Gains (Data.Player "E") shelters
      it "handles trashing" do
        expectParseLine
          "E trashes a Hovel, a Necropolis and an Overgrown Estate."
          $ Data.Trashes (Data.Player "E") shelters
      it "handles exiles" do
        expectParseLine
          "Edi exiles a Horse"
          $ Data.Exiles (Data.Player "Edi") [ Tuple 1 $ Data.Card "Horse" ]
      it "handles shuffle" do
        expectParseLine
          "K shuffles their deck."
          $ Data.Shuffles
          $ Data.Player "K"
      it "handles topdecks" do
        expectParseLine
          "L topdecks 2 Coppers and a Silver."
          $ Data.Topdecks (Data.Player "L")
              [ Tuple 2 $ Data.Card "Copper"
              , Tuple 1 $ Data.Card "Silver"
              ]
      it "handles wishing" do
        expectParseLine
          "E wishes for Ironmonger and finds it."
          $ Data.Draws (Data.Player "E") [ Tuple 1 $ Data.Card "Ironmonger" ]
      it "handles returns" do
        expectParseLine
          "E returns an Estate to the Estate pile."
          $ Data.Returns (Data.Player "E") [ Tuple 1 $ Data.Card "Estate" ]
      it "handles returns with no pile mention" do
        expectParseLine
          "E returns a Horse."
          $ Data.Returns (Data.Player "E") [ Tuple 1 $ Data.Card "Horse" ]
      it "handles looks at" do
        expectParseLine
          "E looks at 3 Estates, 3 Horses, an Ambassador, a Changeling, 5 Coppers and 4 Sleighs."
          $ Data.LooksAt (Data.Player "E")
              [ Tuple 3 $ Data.Card "Estate"
              , Tuple 3 $ Data.Card "Horse"
              , Tuple 1 $ Data.Card "Ambassador"
              , Tuple 1 $ Data.Card "Changeling"
              , Tuple 5 $ Data.Card "Copper"
              , Tuple 4 $ Data.Card "Sleigh"
              ]
      it "handles plays" do
        expectParseLine
          "L plays a Gold and 3 Coppers."
          $ Data.Plays (Data.Player "L")
              [ Tuple 1 $ Data.Card "Gold"
              , Tuple 3 $ Data.Card "Copper"
              ]
      it "handles reveals" do
        expectParseLine
          "E reveals a Mystic and 2 Provinces."
          $ Data.Reveals (Data.Player "E")
              [ Tuple 1 $ Data.Card "Mystic"
              , Tuple 2 $ Data.Card "Province"
              ]
    describe "parseCardsList" do
      it "handles a list of cards separated by commas and an \"and\"" do
        expectSuccessfulParsing Parse.parseCardList
          "a Hovel, a Necropolis and an Overgrown Estate."
          shelters
      it "handles depluralization exceptions" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Nobles, a Hovel."
          [ Tuple 3 $ Data.Card "Nobles"
          , Tuple 1 $ Data.Card "Hovel"
          ]
      it "handles pluralization exceptions" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Emporia, a Hovel."
          [ Tuple 3 $ Data.Card "Emporium"
          , Tuple 1 $ Data.Card "Hovel"
          ]
      it "handles quantities of a card other than one and depluralizes" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Coppers, a Hovel and an Overgrown Estate."
          [ Tuple 3 $ Data.Card "Copper"
          , Tuple 1 $ Data.Card "Hovel"
          , Tuple 1 $ Data.Card "Overgrown Estate"
          ]
      it "can handle a list with no terminating period" do
        expectSuccessfulParsing Parse.parseCardList
          "3 Coppers and 2 Horses"
          [ Tuple 3 $ Data.Card "Copper"
          , Tuple 2 $ Data.Card "Horse"
          ]
