module Main where

import Main
import Prelude

import Color.Scheme.Clrs (blue)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (body, getPageX, getPageY, off, on)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Just), fromMaybe)
import Flare.Drawing (Drawing, render)
import Flare.Drawing as D
import Graphics.Canvas (CANVAS, Context2D, fillRect, getCanvasElementById, getContext2D, setFillStyle)
import Graphics.Isometric (cube, filled, renderScene, rotateX, rotateY, rotateZ, scale)
import Graphics.Isometric.Point as P
import Math (abs, trunc)
import Signal.Time (now)
foreign import getTime:: String
foreign import setFlag :: Boolean -> Boolean
foreign import getFlag :: Boolean -> Boolean
foreign import getSpeed :: Number -> Number
foreign import setSpeed :: Number -> Number
foreign import getUpFlag :: Boolean -> Boolean
foreign import setUpFlag :: Boolean -> Boolean

scene :: Number -> Number -> Number -> Number -> Drawing
scene rotX rotY rotZ time =
  D.translate 500.0 300.0 $
    renderScene { x: -2.0, y: -1.0, z: 0.0 } $
      scale 50.0 $ rotateX rotX $ rotateY rotY $ rotateZ rotZ $
           filled blue $ cube (P.point (-3.0) (-3.0) (-3.0)) 6.0

funcX ::forall c.Partial => Number -> Number -> Eff(canvas:: CANVAS |c) Unit
funcX x y= do
  {-D.runFlareDrawing "controls2" "canvas" $
    scene <$> numberSlider "slideX"  0.0 (2.0* pi) 0.2 0.0
           <*> numberSlider "slideY"  0.0 (2.0* pi) 0.2 0.0
           <*> numberSlider "slide"  0.0 (2.0* pi) 0.2 0.0
           <*> lift animationFrame-}
  Just canvas <- getCanvasElementById "canvas1"
  ctx <- getContext2D canvas
  clearCanvas ctx
  render ctx (scene 0.0 0.0 y 0.0)
clearCanvas :: forall  c. Context2D -> Eff(canvas::CANVAS|c) Unit
clearCanvas ctx = do
  void $ setFillStyle "#ffffff" ctx
  void $ fillRect ctx { x: 0.0, y: 0.0, w: 1500.0, h: 1080.0 }
  pure unit

--main ::forall a. Eff (console :: CONSOLE|a) Unit
main = do
  let zzz=setFlag false
  void $ startStoping
  --_<- startMouseHandlers
  --void $ first 5.0
  log (getTime)

first :: forall a. Partial=> Number -> Eff(canvas::CANVAS|a) Unit
first z =do
  Just canvas <- getCanvasElementById "canvas1"
  ctx <- getContext2D canvas
  clearCanvas ctx
  render ctx (scene 0.0 0.0 5.0 0.0)
startMouseHandlers :: forall a b.Partial => Eff( dom :: DOM, canvas :: CANVAS, console :: CONSOLE, st :: ST a, timer :: TIMER| b)Unit
startMouseHandlers= do
  body <- body
  aa <- newSTRef true
  let downHandler event jq = do
        sX <- getPageX event
        sY <- getPageY event
        startMouse <- newSTRef {x:sX, y: sY}
        t <-(now)
        let startTime=t
            _=setUpFlag false
        --void $ startStoping
        log ("aaaa"<>  show t)
        let moveHandler event1 jq1 = do
              x <- getPageX event1
              y <- getPageY event1
              let dx = negate (y- sY)
              let dy = (x - sX)
              funcX (dx*0.05) (dy* 0.05)
              log (show dx  <> " " <> show dy)


        let upHandler event1 jq1 = do
              x <- getPageX event1
              y <- getPageY event1
              endMouse <- newSTRef {x:x,y:y}
              off "mousemove" body
              te <- now
              let endTime=te
                  _=setUpFlag true
              log (show startTime<> "   "<> show endTime)
              infRotate startTime endTime startMouse endMouse
              off "mouseup" body
        on "mousemove" moveHandler body
        on "mouseup" upHandler body
  on "mousedown" downHandler body

getDecRatio x | x < 10.0 = 2.0
              | x < 50.0 = 10.0
              | x < 100.0 = 20.0
              | x < 200.0 = 30.0
              | x < 400.0 = 50.0
              | x > 500.0 = 60.0
startStoping =do
  let infStop = do
        log ""
        let upF=getUpFlag true 
            oldSpeed=getSpeed 0.0
            decSpeed= getDecRatio $ oldSpeed
            nS= if oldSpeed > 0.0 then (setSpeed (oldSpeed-decSpeed)) else 0.0
        if false
          then pure unit
          else void $ setTimeout 1000 infStop
        pure unit
        log (show oldSpeed <> "   "<> show nS)
  void $ infStop
  log "" 
  


    
infRotate :: forall t105 t112 t225 t91.
  Partial => Number
             -> Number
                -> STRef t91
                     { y :: Number
                     , x :: Number
                     | t105
                     }
                   -> STRef t91
                        { y :: Number
                        , x :: Number
                        | t112
                        }
                      -> Eff
                           ( st :: ST t91
                           , canvas :: CANVAS
                           , console :: CONSOLE
                           , timer :: TIMER
                           | t225
                           )
                           Unit
infRotate st end startM endM= do
  let diff=end-st
  let diffInt=fromNumber diff
  startP <- readSTRef startM
  endP <-readSTRef endM
  let sx=startP.x
      sy=startP.y
      ex=endP.x
      ey=endP.y
  let dist = getDistance sx sy ex ey
      speed = dist / (trunc diff)
      direction= ((ex-sx)/(abs (ex-sx)) / 10.0)
      speedInt=fromMaybe 0 $ fromNumber $ trunc speed
      ss = setSpeed $ speed/10.0
  xx <- newSTRef 0.0

  let tFlag=
        if speed < 1.0
          then 
            setFlag false
          else 
            setFlag true
  let rotateF x= do
        getX <-readSTRef xx
        funcX 0.0 x
        
        --log (show getX)
        let newSpeed= fromMaybe 1 $  fromNumber $ trunc $ getSpeed 0.0
        log ("new Speed"<> show newSpeed)
        --if getX == 360.0 then void $ writeSTRef xx (0.0) else
        --void $ writeSTRef xx (getX+direction)
        let zz= newSpeed > 0
        --log (show zz)
        if zz
          then
            if newSpeed> 2000
              then
                void $ setTimeout (1000/(2000)) $ rotateF $ x+ direction
              else
                void $ setTimeout (10000/(180 * newSpeed)) (rotateF (x+( direction)) )
          else pure unit
  if speed >= 1.0 then (rotateF 0.0) else pure unit
  --log ("speed  "<> show speed)
{-startRotation speed x =do
  xx <- newSTRef x
  let rotateF= do
        getX <-readSTRef xx
        funcX 0.0 (getX+(x))
        log (show getX)
        if getX == 360.0 then void $ writeSTRef xx (0.0) else void $ writeSTRef xx (getX+5.0)
        flag <-readSTRef runFlag
        if speed> 200 && flag  then void $ setTimeout (100000/(360*200)) (rotateF) else void $ setTimeout (100000/(360*speed)) (rotateF)
  rotateF-}

getDistance :: Number -> Number -> Number -> Number -> Number
getDistance sx sy ex ey = (((ex-sx)*(ex-sx))-((ey-sy)*(ey-sy)))
