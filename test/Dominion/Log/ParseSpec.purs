module Dominion.Log.ParseSpec where

import Prelude

import Data.Either (Either(..))
import Dominion.Data as Data
import Dominion.Log.Parse as Parse
import Text.Parsing.Parser (Parser, runParser)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

parseSpec :: Spec Unit
parseSpec = describe "Parsing" do
  let expectSuccessfulParsing parser input expected =
        (runParser input parser) `shouldEqual` (Right expected)
  describe "parseCardsList" do
    it "handles a list of cards separated by commas and an \"and\"" do
      expectSuccessfulParsing Parse.parseCardList
        "a Hovel, a Necropolis and an Overgrown Estate."
        [ Data.Card "Hovel"
        , Data.Card "Necropolis"
        , Data.Card "Overgrown Estate"
        ]
    it "handles quantities of a card" do
      expectSuccessfulParsing Parse.parseCardList
        "3 Coppers, a Hovel and an Overgrown Estate."
        [ Data.Card "Copper"
        , Data.Card "Copper"
        , Data.Card "Copper"
        , Data.Card "Hovel"
        , Data.Card "Overgrown Estate"
        ]
