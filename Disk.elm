module Disk (Model, init, Action, update, view, mouseEventToDiskAction) where

import DragAndDrop
import Effects exposing (Effects)
import Graphics.Input
import Signal exposing (Mailbox, (<~))
import Svg exposing (Svg, circle, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


-- MODEL

type alias Model = 
    { center : Point
    , radius : Float
    , angle : Degrees -- Measured from the top of the disk
    , selected : Bool -- If the disk has been clicked by the mouse
    , selectionStart : Point -- Start of a drag sequence on the disk
    }

type alias Degrees = Float

type alias Radians = Float

type alias Point =
    { x : Float
    , y : Float
    }


init : { x:Float, y:Float, radius:Float } -> (Model, Effects Action)
init {x, y, radius} =
    let
        center = Point x y
        model =
            { center = center
            , radius = radius
            , angle = 0
            , selected = False
            , selectionStart = Point 0 0
            }
    in
        ( model
        , Effects.none
        )


radiansToDegrees : Radians -> Degrees
radiansToDegrees radians = radians * 180/pi


degreesToRadians : Degrees -> Radians
degreesToRadians = degrees


getAngleIndicatorPosition : Model -> Point
getAngleIndicatorPosition {center, radius, angle} =
    { x = center.x + radius * (sin <| degreesToRadians angle)
    , y = center.y - radius * (cos <| degreesToRadians angle)
    }


-- UPDATE

type Action = Rotate Point Radians | Select Point | Unselect


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Rotate rotationCenter angle ->
        let
            translateToOrigin : Point -> Point
            translateToOrigin {x, y} =
                { x = x - rotationCenter.x
                , y = y - rotationCenter.y }
            rotateAboutOrigin : Point -> Point
            rotateAboutOrigin {x, y} =
                { x = x * cos angle - y * sin angle
                , y = x * sin angle + y * cos angle }
            translateBack : Point -> Point
            translateBack {x, y} =
                { x = x + rotationCenter.x
                , y = y + rotationCenter.y }
            doRotation : Point -> Point
            doRotation point =
                point |> translateToOrigin
                      |> rotateAboutOrigin
                      |> translateBack
            newCenter : Point
            newCenter =
                doRotation model.center
            getAngleOnDisk : Point -> Radians
            getAngleOnDisk {x, y} =
                radiansToDegrees <| pi/2 + atan2 (y - newCenter.y) (x - newCenter.x)
            newAngle : Radians
            newAngle =
                model |> getAngleIndicatorPosition
                      |> doRotation
                      |> getAngleOnDisk
            newModel : Model
            newModel =
                { model | center <- newCenter
                        , angle <- newAngle }
        in
            ( newModel, Effects.none )
    Select point ->
        let
            newModel = { model | selected <- True
                               , selectionStart <- point }
        in
            ( newModel, Effects.none )
    Unselect ->
        let
            newModel = { model | selected <- False }
        in
            ( newModel, Effects.none )


toPoint : (Int, Int) -> Point
toPoint (x, y) =
    Point (toFloat x) (toFloat y)


within : Point -> Model -> Bool
within point model =
    let
        distance point1 point2 = sqrt <| (point1.x - point2.x)^2 + (point1.y - point2.y)^2 
    in
        distance point model.center <= model.radius


mouseEventToDiskAction : Model -> DragAndDrop.MouseEvent -> Action
mouseEventToDiskAction model action =
    case action of
        DragAndDrop.StartAt origin ->
            let
                originPoint =
                    toPoint origin
            in
                if originPoint `within` model then
                    Select originPoint
                else
                    Unselect
        DragAndDrop.MoveFromTo origin destination ->
            if model.selected then
                let
                    originPoint =
                        toPoint origin
                    destinationPoint =
                        toPoint destination
                    angle1 =
                        atan2 (originPoint.y - model.selectionStart.y) (originPoint.x - model.selectionStart.x)
                    angle2 =
                        atan2 (destinationPoint.y - model.selectionStart.y) (destinationPoint.x - model.selectionStart.x)
                in
                    Rotate model.center (angle1 - angle2)
            else
                Unselect
        DragAndDrop.EndAt destination ->
            Unselect


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
