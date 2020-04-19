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
expectSuccessfulParsing parser input expected =
  (runParser input parser) `shouldEqual` (Right expected)

parseSpec :: Spec Unit
parseSpec = describe "Parsing" do
  let shelters = [ Tuple 1 $ Data.Card "Hovel"
                 , Tuple 1 $ Data.Card "Necropolis"
                 , Tuple 1 $ Data.Card "Overgrown Estate"
                 ]
  describe "parseLine" do
    it "handles gains" do
      expectSuccessfulParsing Parse.parseLine
        "E starts with a Hovel, a Necropolis and an Overgrown Estate."
        [(Data.Gains (Data.Player "E") shelters)]
    it "handles trashing" do
      expectSuccessfulParsing Parse.parseLine
        "E trashes a Hovel, a Necropolis and an Overgrown Estate."
        [(Data.Trashes (Data.Player "E") shelters)]
    it "handles exiles" do
      expectSuccessfulParsing Parse.parseLine
        "Edi exiles a Horse"
        [(Data.Exiles (Data.Player "Edi") [ Tuple 1 $ Data.Card "Horse" ])]
  describe "parseCardsList" do
    it "handles a list of cards separated by commas and an \"and\"" do
      expectSuccessfulParsing Parse.parseCardList
        "a Hovel, a Necropolis and an Overgrown Estate."
        shelters
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
