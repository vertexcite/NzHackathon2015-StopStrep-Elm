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
import Random (..)

l !! n = List.head (List.drop n l)

spriteSize = 70
strepMaxY = 200
strepStartY0 = 20
strepStartY1 = 40
strepStartY2 = 20
strepStartX0 = -100
strepStartX1 = 0
strepStartX2 = 100
strepStartXs = [strepStartX0, strepStartX1, strepStartX2]
strepStartYs = [strepStartY0, strepStartY1, strepStartY2]
-- MODEL

type alias BicillinSprite = { x:Float, y:Float, zapped:Bool, fizzled:Bool, image:String}

strep0 = { x=strepStartX0, y=strepStartY0, zapped= False, fizzled=False, image="media/Strep01.png"}
strep1 = { x=strepStartX1, y=strepStartY1, zapped= False, fizzled=False, image="media/Strep02.png" }
strep2 = { x=strepStartX2, y=strepStartY2, zapped= False, fizzled=False, image="media/Strep03.png" }

type alias World = {seed: Seed, targets:List BicillinSprite}

world : World
world = {seed = initialSeed 3103, targets = [strep0, strep1, strep2]}


-- UPDATE -- ("w" is for World)
scroll : Float -> World -> World
scroll t w = {w | targets <- List.filter (\s -> s.fizzled == False) << List.map (scrollEach t) <|  w.targets}
scrollEach t s = if s.y > strepMaxY then { s | fizzled <- True } else {s | y <- s.y + t}

zap : Maybe (Int, Int) -> World -> World
zap touch w = {w | targets <- List.map (zapEach touch) w.targets}
zapEach touch s = Maybe.withDefault s ( Maybe.map (zap2 s) touch)
zap2 s (x,y) = if (close (round s.x) x) && (close (round s.y) y) then {s | zapped <- True} else s

tolerance : Int
tolerance = 40
close : Int -> Int -> Bool
close target actual = (actual > target - tolerance) && (actual < target + tolerance)

step : (Float, List Touch.Touch, (Int, Int)) -> World -> World
step (dt, touches, (width,height)) =
  let
    touch = if | List.isEmpty touches -> Nothing
               | otherwise -> Just ((List.head touches).x - round ((toFloat width)/2), round ((toFloat height)/2) - (List.head touches).y  )
--    zapped = zap touch world
--    scrolled = scroll dt zapped
  in possiblyAddBadGuy dt << scroll dt << zap touch
--  in {world | targets <- scrolled}


addProbability = 0.01


possiblyAddBadGuy : Float -> World -> World
possiblyAddBadGuy dt world =
    let (choice, seed') =
          generate (float 0 1) world.seed
    in
        if choice < addProbability * dt
          then
            let (position, seed'') = generate (pair (float (-100) 100) (float (-100) 100)) seed'
            in
                { world | targets <- { x= fst position, y=snd position, zapped= False, fizzled=False, image="media/Strep01.png"} :: world.targets,
                          seed <- seed''
                }
          else
            { world |
                seed <- seed'
            }

-- DISPLAY
render (w',h') world =
  let (w,h) = (toFloat w', toFloat h')
  in collage w' h' (
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
--      , rect (toFloat (2*tolerance)) (toFloat (2*tolerance)) |> filled (rgb 255 255 255) |> move (0, world.strepY) -- Render hit zone
      , toForm (image 58 86 "media/Injection.png") |> move (0, 0 + 62 - h/2)
--      , toForm (image spriteSize spriteSize "media/Strep02.png") |> move (100, world.strepY + 50) 
--      , toForm (image spriteSize spriteSize "media/Strep03.png") |> move (-100, world.strepY + 100) 
      , toForm (image spriteSize spriteSize "media/Heart.png") |> move (0, strepMaxY) 
      ] `List.append` (List.map renderBicillinSprite world.targets) ) 
renderBicillinSprite s = toForm (image spriteSize spriteSize (if s.zapped then "media/BicillinTopUp.png" else s.image)) |> move (s.x, s.y)

-- WORLD
input = let delta = Signal.map (\t -> t/20) (fps 25)
        in  Signal.sampleOn delta (Signal.map3 (,,) delta Touch.touches Window.dimensions)

main = Signal.map2 render Window.dimensions (Signal.foldp step world input)