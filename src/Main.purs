module Main where


import qualified Thermite as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

import Data.Array
import Data.Traversable
import Data.Maybe
import Data.Foldable

type Line = Array Int

type Game = Array Line

data Action = NextLine


render :: T.Render Game _ Action
render ctx s _ _ = [R.div' [R.text "ho"]] -- map renderLine s)
    -- where renderLine line = R.ul'  (R.text "hi") -- (map (R.li' <<< R.text) line)

initGame :: forall e. Int -> Eff (random :: RANDOM | e) Game
initGame n = do
  xs <- sequence $ map (const (map f random)) l
  return [xs]
    where l = (0 .. n)
          f x = if (x < 0.5) then 0 else 1


performAction :: T.PerformAction _ Game _ Action
performAction _ _ state update = update state

spec :: T.Spec _ Game _ Action
spec = T.simpleSpec performAction render

main = do
  let component = T.createClass spec [[1,0,1]]
  R.body >>= R.render (R.createFactory component {})

