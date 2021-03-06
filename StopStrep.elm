module StopStrep where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal
import Time exposing (..)
import Window
import Touch
import List
import Maybe
import Random exposing (..)

spriteToSpanRatio : Float
spriteToSpanRatio = 1/10
proportionedSpriteSize : Int -> Int -> Int
proportionedSpriteSize width height = round (spriteToSpanRatio * toFloat (min width height))
strepMaxY : number
strepMaxY = 200
-- MODEL

type alias BicillinSprite = { x:Float, y:Float, zapped:Bool, fizzled:Bool, image:String}

type alias World = {seed: Seed, targets:List BicillinSprite}

-- UPDATE -- ("w" is for World)
scroll : Float -> World -> World
scroll t w = {w | targets = List.filter (\s -> not s.fizzled) << List.map (scrollEach t) <|  w.targets}

scrollEach : number -> BicillinSprite -> BicillinSprite
scrollEach t s = if s.y > strepMaxY then { s | fizzled = True } else {s | y = s.y + t}

zap : Maybe (Int, Int) -> (Int, Int) -> World -> World
zap touch (width, height) w =
  let spriteSize = proportionedSpriteSize width height
  in {w | targets = List.map (zapEach spriteSize touch) w.targets}

zapEach : Int -> Maybe (Int, Int) -> BicillinSprite -> BicillinSprite
zapEach spriteSize touch s = Maybe.withDefault s ( Maybe.map (zap2 spriteSize s) touch)

zap2 : Int -> BicillinSprite -> (Int, Int) -> BicillinSprite
zap2 spriteSize s (x,y) = if (close spriteSize (round s.x) x) && (close spriteSize (round s.y) y) then {s | zapped = True} else s

close : Int -> Int -> Int -> Bool
close spriteSize target actual = 2 * abs (actual - target) < spriteSize

step : (Float, List Touch.Touch, (Int, Int)) -> World -> World
step (dt, touches, (width,height)) =
  let
    touch =
      case List.head touches of
        Nothing -> Nothing
        Just t -> Just (t.x - round ((toFloat width)/2), round ((toFloat height)/2) - t.y  )
  in possiblyAddBadGuy dt (width, height) << scroll dt << zap touch (width, height)


addProbability : Float
addProbability = 0.01
yMarginRatio : Float
yMarginRatio = 0.2

possiblyAddBadGuy : Float -> (Int, Int) -> World -> World
possiblyAddBadGuy dt (width, height) world =
    let (choice, seed') =
          generate (float 0 1) world.seed
    in
        if (List.length world.targets == 0) || (choice < addProbability * dt)
          then
            let
              xRange = toFloat width / 2
              (position, seed'') = generate (pair (float (-xRange) xRange) (float (-(1 - yMarginRatio) / 2 * toFloat height) 0)) seed'
            in
                { world | targets = { x= fst position, y=snd position, zapped= False, fizzled=False, image="media/Strep01.png"} :: world.targets,
                          seed = seed''
                }
          else
            { world |
                seed = seed'
            }

-- DISPLAY
render : (Int, Int) -> World -> Element
render (w',h') world =
  let
    (w,h) = (toFloat w', toFloat h')
    spriteSize = proportionedSpriteSize w' h'
    renderBicillinSprite s = toForm (image spriteSize spriteSize (if s.zapped then "media/BicillinTopUp.png" else s.image)) |> move (s.x, s.y)
  in collage w' h' (
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
--      , rect (toFloat (2*tolerance)) (toFloat (2*tolerance)) |> filled (rgb 255 255 255) |> move (0, world.strepY) -- Render hit zone
      , toForm (image 58 86 "media/Injection.png") |> move (0, 0 + 62 - h/2)
      , toForm (image spriteSize spriteSize "media/Heart.png") |> move (0, strepMaxY) 
      ] `List.append` (List.map renderBicillinSprite world.targets) ) 


-- WORLD
input : Signal (Float, List Touch.Touch, (Int, Int))
input = let delta = Signal.map (\t -> t/20) (fps 25)
        in  Signal.sampleOn delta (Signal.map3 (,,) delta Touch.touches Window.dimensions)

