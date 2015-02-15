import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import Window
import Touch
import List
import Maybe

spriteSize = 70
strepMaxY = 200
strepStartY = 20

-- MODEL


world = { x=0, y=0, vx=0, vy=0, dir="right", strepY=strepStartY, zapped= False }


-- UPDATE -- ("m" is for World)
jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/16 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x
                 , dir <- if x < 0 then "left" else
                          if x > 0 then "right" else m.dir }
scroll t m = if m.strepY > strepMaxY then { m | strepY <- strepStartY, zapped <- False } else {m | strepY <- m.strepY + t}
zap touch m = Maybe.withDefault m ( Maybe.map (zap2 m) touch)
zap2 m (x,y) = if (close 0 x) && (close (round m.strepY) y) then {m | zapped <- True} else m

tolerance : Int
tolerance = 40
close : Int -> Int -> Bool
close target actual = (actual > target - tolerance) && (actual < target + tolerance)


step (dt, keys, touches, (w,h)) =
  let
    touch = if | List.isEmpty touches -> Nothing
               | otherwise -> Just ((List.head touches).x - round ((toFloat w)/2), round ((toFloat h)/2) - (List.head touches).y  )
  in jump keys >> gravity dt >> walk keys >> physics dt >> scroll dt >> zap touch


-- DISPLAY
render (w',h') world =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | world.y  >  0 -> "jump"
                | world.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "media/Injection.png"
  in collage w' h'
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
--      , rect (toFloat (2*tolerance)) (toFloat (2*tolerance)) |> filled (rgb 255 255 255) |> move (0, world.strepY) -- Render hit zone
      , toForm (image 58 86 src) |> move (world.x, world.y + 62 - h/2)
      , toForm (image spriteSize spriteSize (if world.zapped then "media/BicillinTopUp.png" else "media/Strep01.png")) |> move (0, world.strepY) 
      , toForm (image spriteSize spriteSize "media/Strep02.png") |> move (100, world.strepY + 50) 
      , toForm (image spriteSize spriteSize "media/Strep03.png") |> move (-100, world.strepY + 100) 
      , toForm (image spriteSize spriteSize "media/Heart.png") |> move (0, strepMaxY) 
      ]

-- WORLD
input = let delta = Signal.map (\t -> t/20) (fps 25)
        in  Signal.sampleOn delta (Signal.map4 (,,,) delta Keyboard.arrows Touch.touches Window.dimensions)

main = Signal.map2 render Window.dimensions (Signal.foldp step world input)