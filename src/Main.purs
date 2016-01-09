module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

import Data.Array
import Data.Traversable
import Data.Maybe
import Data.Foldable
import Data.String (fromCharArray)

type Line = Array Int

type Game = Array Line

-- [l] --  sequence <<< (map (const randomBool) l)

initGame :: forall e. Int -> Eff (random :: RANDOM | e) Game
initGame n = do
  xs <- sequence $ map (const (map f random)) l
  return [xs]
  where l = (0 .. n)
        f x = if (x < 0.9) then 0 else 1

rule110 [0, 0, 0] = 0
rule110 [0, 0, 1] = 1
rule110 [0, 1, 0] = 1
rule110 [0, 1, 1] = 1
rule110 [1, 0, 0] = 0
rule110 [1, 0, 1] = 1
rule110 [1, 1, 0] = 1
rule110 [1, 1, 1] = 0

rule124 [0, 0, 0] = 0
rule124 [0, 0, 1] = 1
rule124 [0, 1, 0] = 1
rule124 [0, 1, 1] = 1
rule124 [1, 0, 0] = 1
rule124 [1, 0, 1] = 1
rule124 [1, 1, 0] = 1
rule124 [1, 1, 1] = 0

rule = rule124

triplesFromArray ary0 = do
  i <- 0 .. (length ary - 3)
  return (slice i (i+3) ary)
  where ary = cons 0 $ snoc ary0 0

nextState ary = map rule $ triplesFromArray ary

developGame n [state] = develop n state

develop 0 state = [state]
develop n state = state : develop (n-1) (nextState state)

nice 0 = ' '
nice 1 = 'x'

printNice xs = print $ fromCharArray $ map nice xs

test = do
  sequence $ map printNice $
    Main.develop 150 (snoc (replicate 150 0) 1)
  return unit

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  test
