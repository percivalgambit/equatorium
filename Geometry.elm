module Geometry (Degrees, Radians, Point, Circle, radiansToDegrees,
                 degreesToRadians, tupleToPoint, distance, getAngleBetweenPoints,
                 rotateAboutPoint, getPointOnCircle, getAngleOnCircle,
                 withinCircle) where

type alias Degrees = Float

type alias Radians = Float

type alias Point =
    { x : Float
    , y : Float
    }

type alias Circle circle = { circle | center : Point, radius : Float }


radiansToDegrees : Radians -> Degrees
radiansToDegrees radians = radians * 180/pi


degreesToRadians : Degrees -> Radians
degreesToRadians = degrees


tupleToPoint : (Int, Int) -> Point
tupleToPoint (x, y) =
    Point (toFloat x) (toFloat y)


distance : Point -> Point -> Float
distance point1 point2 =
    sqrt <| (point1.x - point2.x)^2 + (point1.y - point2.y)^2


getAngleBetweenPoints : Point -> Point -> Point -> Radians
getAngleBetweenPoints fixedPoint origin destination =
    let
        angle1 =
            atan2 (destination.y - fixedPoint.y) (destination.x - fixedPoint.x)
        angle2 =
            atan2 (origin.y - fixedPoint.y) (origin.x - fixedPoint.x)
    in
        angle1 - angle2

rotateAboutPoint : Point -> Radians -> Point -> Point
rotateAboutPoint rotationCenter angle =
    let
        translateToOrigin {x, y} =
            { x = x - rotationCenter.x
            , y = y - rotationCenter.y }
        rotateAboutOrigin {x, y} =
            { x = x * cos angle - y * sin angle
            , y = x * sin angle + y * cos angle }
        translateBack {x, y} =
            { x = x + rotationCenter.x
            , y = y + rotationCenter.y }
        doRotation =
            translateToOrigin
            >> rotateAboutOrigin
            >> translateBack
    in
        doRotation


getPointOnCircle : Circle a -> Radians -> Point
getPointOnCircle {center, radius} angle =
    { x = center.x + radius * sin angle
    , y = center.y - radius * cos angle
    }


getAngleOnCircle : Circle a -> Point -> Radians
getAngleOnCircle {center, radius} {x, y} =
    pi/2 + atan2 (y - center.y) (x - center.x)


withinCircle : Point -> Circle a -> Bool
withinCircle point {center, radius} =
    distance point center <= radius
