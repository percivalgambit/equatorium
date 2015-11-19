module Disk (Model, init, Action, update, view) where

import Svg exposing (Svg, circle, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

-- MODEL

type alias Model = 
    { center : Point
    , radius : Float
    , angle : Radians -- Measured from the top of the disk
    }

type alias Radians = Float

type alias Point = 
    { x : Float
    , y : Float
    }

init : { x:Float, y:Float, radius:Float } -> Model
init {x, y, radius} = 
    { center = Point x y
    , radius = radius
    , angle = 0
    }

getAngleIndicatorPosition : Model -> Point
getAngleIndicatorPosition {center, radius, angle} =
    { x = center.x + radius * sin angle
    , y = center.y - radius * cos angle
    }

-- UPDATE

type Action = Rotate Point Float

update : Action -> Model -> Model
update action model =
  case action of
    Rotate rotationCenter angle ->
        let
            translateToOrigin : Point -> Point
            translateToOrigin {x, y} = { x = x - rotationCenter.x
                                       , y = y - rotationCenter.y }
            rotateAboutOrigin : Point -> Point
            rotateAboutOrigin {x, y} = { x = x * cos angle - y * sin angle
                                       , y = x * sin angle + y * cos angle }
            translateBack : Point -> Point
            translateBack {x, y} = { x = x + rotationCenter.x
                                   , y = y + rotationCenter.y }
            doRotation : Point -> Point
            doRotation point = point |> translateToOrigin
                                     |> rotateAboutOrigin
                                     |> translateBack
            newCenter : Point
            newCenter = doRotation model.center
            getAngleOnDisk : Point -> Radians
            getAngleOnDisk {x, y} =
                pi/2 + atan2 (y - newCenter.y) (x - newCenter.x)
            newAngle : Radians
            newAngle = model |> getAngleIndicatorPosition
                             |> doRotation
                             |> getAngleOnDisk
        in
            { model | center <- newCenter
                    , angle <- newAngle }


-- VIEW

view : Signal.Address Action -> Model -> Svg
view address model =
    let
        disk = 
            circle
                [ cx << toString <| model.center.x
                , cy << toString <| model.center.y
                , r << toString <| model.radius
                , fill "#FFFFFF"
                , stroke "black"
                , strokeWidth "1"
                ]
                []
        angleIndicatorPosition = getAngleIndicatorPosition model
        angleIndicator =
            circle
                [ cx << toString <| angleIndicatorPosition.x
                , cy << toString <| angleIndicatorPosition.y
                , r << toString <| model.radius / 13
                , style "fill: #0000FF;"
                ]
                []
    in
        g [ onClick (Signal.message address (Rotate model.center (pi/2))) ]
          [ disk, angleIndicator ]