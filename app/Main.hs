{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Wordell.Game

import Polysemy
import Polysemy.State (evalState)

import Data.Function ((&))
import qualified Data.Text.IO as T
import qualified Data.Text as T

import System.Random (randomRIO)

main :: IO ()
main = game
  & evalState []
  & randomWordToIOFinal
  & wordleDisplayToIOFinal
  & embedToFinal @IO
  & runFinal

randomWordToIOFinal :: Member (Embed IO) r => Sem (RandomWord ': r) a -> Sem r a
randomWordToIOFinal = interpret \case
  PickWord -> embed do
    dictionary <- T.lines <$> T.readFile "dictionary"
    n <- randomRIO (0, length dictionary - 1)
    return (GameWord $ dictionary !! n)

wordleDisplayToIOFinal :: Member (Embed IO) r => Sem (WordleDisplay ': r) a -> Sem r a
wordleDisplayToIOFinal = interpret \case
  ReadFromDisplay -> embed T.getLine
  WriteToDisplay t -> embed (T.putStrLn t)

