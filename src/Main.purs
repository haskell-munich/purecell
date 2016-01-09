module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

import Data.Array
import Data.Traversable
import Data.Maybe
import Data.Foldable

type Line = Array Number

type Game = Array Line

-- [l] --  sequence <<< (map (const randomBool) l)

initGame :: forall e. Int -> Eff (random :: RANDOM | e) Game
initGame n = do
  xs <- sequence $ map (const random) l
  return [xs]
  where l = (0 .. n)


main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  log "Hello sailor!"
