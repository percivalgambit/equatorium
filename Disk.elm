module Disk (init, update, view) where

import Svg exposing (Svg, circle, svg, g, text', text)
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

init : (Float, Float) -> Float -> Model
init (x, y) radius =
    { center = { x = x, y = y }
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
        in
            { model | center <- newCenter
                    , angle <- getAngleOnDisk <| doRotation
                                              <| getAngleIndicatorPosition
                                              <| model }


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
        svg
            [ width << toString <| model.center.x + model.radius * 1.1
            , height << toString <| model.center.y + model.radius * 1.1
            ]
            [ g []
                [ disk, angleIndicator ]
            ]