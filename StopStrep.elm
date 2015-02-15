import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import Window
import Touch

spriteSize = 70
strepMaxY = 200
strepStartY = 20

-- MODEL
world = { x=0, y=0, vx=0, vy=0, dir="right", strepY=strepStartY }


-- UPDATE -- ("m" is for World)
jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x
                 , dir <- if x < 0 then "left" else
                          if x > 0 then "right" else m.dir }
scroll t m = if m.strepY > strepMaxY then { m | strepY <- strepStartY } else {m | strepY <- m.strepY + t}

step (dt, keys) =
  jump keys >> gravity dt >> walk keys >> physics dt >> scroll dt


-- DISPLAY
render (w',h') world _ =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | world.y  >  0 -> "jump"
                | world.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "media/BicillinTopUp.png"
  in collage w' h'
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
      , toForm (image 58 86 src) |> move (world.x, world.y + 62 - h/2)
      , toForm (image spriteSize spriteSize "media/Strep01.png") |> move (0, world.strepY) 
      , toForm (image spriteSize spriteSize "media/Heart.png") |> move (0, strepMaxY) 
      ]

-- WORLD
input = let delta = Signal.map (\t -> t/20) (fps 25)
        in  Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

main = Signal.map3 render Window.dimensions (Signal.foldp step world input) Touch.touches
