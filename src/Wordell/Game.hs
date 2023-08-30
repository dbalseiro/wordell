module Wordell.Game (game) where

import Data.Text (Text)

data Outcome = Won | Wrong | OutOfTries

game :: a
game = do
  word <- pickWord
  playTurn word

playTurn :: Text -> a
playTurn word = do
  displayGuesses
  guess <- askGuess
  outcome <- calculateOutcome word guess
  case outcome of
    Won -> displayWinnerMessage
    OutOfTries -> displayLoserMessage
    Wrong -> playTurn word

