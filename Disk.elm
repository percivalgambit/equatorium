-- Represents a single disk in the model of an Equatorium.

module Disk (Model, init, Action(..), update, view, mouseEventToDiskAction) where

import Geometry exposing (Radians, Point, rotateAboutPoint, getPointOnCircle,
                          getAngleOnCircle, tupleToPoint, withinCircle,
                          getAngleBetweenPoints, radiansToDegrees)

import DragAndDrop
import Svg exposing (Svg, circle, defs, pattern, image)
import Svg.Attributes exposing (id, patternUnits, height, width, xlinkHref,
                                x, y, transform, cx, cy, r, fill)


-- MODEL

type alias Model =
    { center : Point
    , radius : Float
    , background : String
    , angle : Radians -- Measured from the top of the disk
    , selected : Bool -- If the disk has been clicked by the mouse
    }


init : { x:Float, y:Float, radius:Float, background:String } -> Model
init {x, y, radius, background} =
    let
        center = Point x y

        model =
            { center = center
            , radius = radius
            , background = background
            , angle = 0
            , selected = False
            }
    in
        model


-- UPDATE

type Action
    = Rotate Point Radians -- Rotates the disk about the given point.
    | Select
    | Unselect


update : Action -> Model -> Model
update action model =
  case action of
    Rotate rotationCenter angle ->
        let
            newCenter =
                rotateAboutPoint rotationCenter angle model.center

            circleWithNewCenter =
                { center = newCenter
                , radius = model.radius
                }

            newAngle =
                model
                    |> flip getPointOnCircle model.angle
                    |> rotateAboutPoint rotationCenter angle
                    |> getAngleOnCircle circleWithNewCenter

            newModel =
                { model
                    | center = newCenter
                    , angle = newAngle
                }
        in
            newModel

    Select ->
        { model
            | selected = True
        }

    Unselect ->
        { model
            | selected = False
        }


-- Converts a MousEvent from the DragAndDrop package to an action on a Disk.
mouseEventToDiskAction : DragAndDrop.MouseEvent -> Model -> Maybe Action
mouseEventToDiskAction action model =
    case action of
        DragAndDrop.StartAt origin ->
            let
                originPoint =
                    tupleToPoint origin
            in
                if originPoint `withinCircle` model then
                    Just Select
                else
                    Nothing

        DragAndDrop.MoveFromTo origin destination ->
            if model.selected then
                let
                    originPoint =
                        tupleToPoint origin

                    destinationPoint =
                        tupleToPoint destination

                    rotationAngle =
                        getAngleBetweenPoints model.center originPoint destinationPoint
                in
                    Just <| Rotate model.center rotationAngle
            else
                Nothing

        DragAndDrop.EndAt destination ->
            if model.selected then
                Just Unselect
            else
                Nothing


-- VIEW

view : Signal.Address Action -> Model -> List Svg
view address model =
    let
        background =
            defs
                []
                [ pattern
                        [ id model.background
                        , patternUnits "userSpaceOnUse"
                        , height <| toString <| model.center.y + model.radius
                        , width <| toString <| model.center.x + model.radius
                        ]
                        [ image
                            [ xlinkHref model.background
                            , height <| toString <| model.radius*2
                            , width <| toString <| model.radius*2
                            , x <| toString <| model.center.x - model.radius
                            , y <| toString <| model.center.y - model.radius
                            , transform <|
                                "rotate("
                                 ++ toString (radiansToDegrees model.angle)
                                 ++ ", " ++ toString model.center.x
                                 ++ ", " ++ toString model.center.y ++ ")"
                            ]
                            []
                        ]
                ]
        disk =
            circle
                [ cx <| toString model.center.x
                , cy <| toString model.center.y
                , r <| toString model.radius
                , fill <| "url(#" ++ model.background ++ ")"
                ]
                []
    in
        [ background, disk ]
