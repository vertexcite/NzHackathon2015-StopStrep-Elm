module EmbeddedMain where

import Graphics.Element exposing (..)
import Window
import Random exposing (..)
import StopStrep exposing (render, step, input, World)

port portSeed : Int

world : World
world = {seed = initialSeed portSeed, targets = []}

main : Signal Element
main = Signal.map2 render Window.dimensions (Signal.foldp step world input)
