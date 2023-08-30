{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Polysemy
import Polysemy.State
import Polysemy.Fail

import Data.Text (Text)
import Data.Function ((&))

import Control.Arrow (second)

import Wordell.Game

newtype Input = Input Text
newtype Output = Output Text deriving Eq

main :: IO ()
main = hspec do
  describe "askGuess" $ do
    it "returns a guess" $ do
      let result = snd <$> runAskGuess [] [Input "abcde"]
      result `shouldBe` Right "abcde"

    context "when the input is more than 5 letters" $
      it "displays a custom message" $ do
        let display = fst <$> runAskGuess [] [Input "abcdef", Input "abcde"]
        fmap (elem (Output "Only 5-letter words, please")) display `shouldBe` Right True

    context "when the input alredy exists" $
      it "displays a custom message" $ do
        let display = fst <$> runAskGuess ["abcde"] [Input "abcde", Input "aaaaa"]
        fmap (elem (Output "Already tried that")) display `shouldBe` Right True


runAskGuess :: [Text] -> [Input] -> Either String ([Output], Text)
runAskGuess guesses inputs = askGuess
  & evalState guesses
  & testWordleDisplay
  & evalState inputs
  & runState ([] :: [Output])
  & runFail
  & run
  & fmap (second unGuess)

testWordleDisplay :: Members '[State [Input], State [Output], Fail] r => Sem (WordleDisplay ': r) a -> Sem r a
testWordleDisplay = interpret \case
  ReadFromDisplay -> do
    (Input i : xs) <- get
    put xs
    return i
  WriteToDisplay t -> modify (Output t:)
