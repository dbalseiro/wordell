module Main (main) where

import Wordell.Game (game)

import Polysemy (runFinal)

main :: IO ()
main = runFinal game
