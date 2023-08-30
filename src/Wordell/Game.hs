{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Wordell.Game (game, GameWord(..), RandomWord(..), WordleDisplay(..)) where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (when)

import Polysemy
import Polysemy.State
import Polysemy.Error

data Outcome = Won | Wrong | OutOfTries

newtype GameWord = GameWord Text
newtype Guess = Guess { unGuess :: Text }

data RandomWord m a where
  PickWord :: RandomWord m GameWord

-- pickWord :: Member RandomWord r => Sem r Text
makeSem ''RandomWord

data WordleDisplay m a where
  ReadFromDisplay :: WordleDisplay m Text
  WriteToDisplay :: Text -> WordleDisplay m ()

makeSem ''WordleDisplay

game :: Members '[RandomWord, WordleDisplay, State [Text]] r => Sem r ()
game = pickWord >>= playTurn

playTurn :: Members '[WordleDisplay, State [Text]] r => GameWord -> Sem r ()
playTurn word = do
  displayGuesses
  guess <- askGuess
  outcome <- calculateOutcome guess word
  case outcome of
    Won -> displayWinnerMessage
    OutOfTries -> displayLoserMessage
    Wrong -> do
      modify (unGuess guess:)
      playTurn word

askGuess :: Members '[WordleDisplay, State [Text]] r => Sem r Guess
askGuess = do
  writeToDisplay "Guess? "
  s <- readFromDisplay
  either reask (\() -> return $ Guess s) =<< runError do
      validateLength s
      validateState s
  where
    reask :: Members '[WordleDisplay, State [Text]] r => Text -> Sem r Guess
    reask s = do
      writeToDisplay s
      askGuess

    validateLength :: Member (Error Text) r => Text -> Sem r ()
    validateLength s =
      when (T.length s /= 5) $
        throw "Only 5-letter words, please"

    validateState :: Members '[Error Text, State [Text]] r => Text -> Sem r ()
    validateState s = do
      l <- get
      when (s `elem` l) $
        throw "Already tried that"

displayGuesses :: Members '[WordleDisplay, State [Text]] r => Sem r ()
displayGuesses = get >>= mapM_ writeToDisplay

displayWinnerMessage :: Member WordleDisplay r => Sem r ()
displayWinnerMessage = writeToDisplay "You won :)"

displayLoserMessage :: Member WordleDisplay r => Sem r ()
displayLoserMessage = writeToDisplay "Loser!!"

calculateOutcome :: Member (State [Text]) r => Guess -> GameWord -> Sem r Outcome
calculateOutcome (Guess g) (GameWord w)
  | g == w = return Won
  | otherwise = do
    l <- gets length
    return $ if l >= 5 then OutOfTries else Wrong

