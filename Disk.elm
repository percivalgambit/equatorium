module Disk where

import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (cx, cy, r, fill, stroke, strokeWidth, width, height)

-- MODEL

type alias Model = 
    { center : Point
    , radius : Float
    , angle : Degrees -- Measured from the top of the disk
    }

type alias Degrees = Float

type alias Point = 
    { x : Float
    , y : Float
    }

init : Point -> Float -> Model
init center radius =
    { center = center
    , radius = radius
    , angle = 0
    }


-- UPDATE

type Action = Rotate Point Float

update : Action -> Model -> Model
update action model =
  case action of
    Rotate rotation_center degrees ->
        model


-- VIEW

view : Signal.Address Action -> Model -> Svg
view address model =
    svg
        [width "1000", height "1000"]
        [ circle
            [ cx (toString model.center.x)
            , cy (toString model.center.y)
            , r (toString model.radius)
            , fill "none"
            , stroke "black"
            , strokeWidth "1"
            ]
            []
        ]