-- Stores miscellaneous functions to calculate the positions of points and angles.
-- Note: All angles in this module are measured from the top of the corresponding
--       circle.

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


type alias Circle circle =
    { circle | center : Point, radius : Float }


-- Takes in a value in radians and returns the corresponding number of degrees.
radiansToDegrees : Radians -> Degrees
radiansToDegrees radians = radians * 180/pi


-- Take in a value in degrees and returns the corresponding number of radians.
degreesToRadians : Degrees -> Radians
degreesToRadians = degrees


-- Transforms a tuple of Ints into a Point.
tupleToPoint : (Int, Int) -> Point
tupleToPoint (x, y) =
    Point (toFloat x) (toFloat y)


-- Finds the distance between two points.
distance : Point -> Point -> Float
distance point1 point2 =
    sqrt <| (point1.x - point2.x)^2 + (point1.y - point2.y)^2


-- Takes in a fixed point and two other points and finds the angle between the
-- two points relative to the fixed point.
getAngleBetweenPoints : Point -> Point -> Point -> Radians
getAngleBetweenPoints fixedPoint origin destination =
    let
        angle1 =
            atan2 (destination.y - fixedPoint.y) (destination.x - fixedPoint.x)

        angle2 =
            atan2 (origin.y - fixedPoint.y) (origin.x - fixedPoint.x)
    in
        angle1 - angle2


-- Takes in the center of a rotation and the angle of a rotation and returns a
-- corresponding rotation function.
rotateAboutPoint : Point -> Radians -> (Point -> Point)
rotateAboutPoint rotationCenter angle =
    let
        translateToOrigin {x, y} =
            { x = x - rotationCenter.x
            , y = y - rotationCenter.y
            }

        rotateAboutOrigin {x, y} =
            { x = x * cos angle - y * sin angle
            , y = x * sin angle + y * cos angle
            }

        translateBack {x, y} =
            { x = x + rotationCenter.x
            , y = y + rotationCenter.y
            }

        doRotation =
            translateToOrigin >> rotateAboutOrigin >> translateBack
    in
        doRotation


-- Returns the point on the edge of the given circle corresponding to the given
-- angle.
-- Note: the given angle should be measured from the top of the circle.
getPointOnCircle : Circle a -> Radians -> Point
getPointOnCircle {center, radius} angle =
    { x = center.x + radius * sin angle
    , y = center.y - radius * cos angle
    }


-- Returns the angle from the top of the given circle where the given point is
-- located.  The given point must be located on the edge of the given circle.
getAngleOnCircle : Circle a -> Point -> Radians
getAngleOnCircle {center, radius} {x, y} =
    pi/2 + atan2 (y - center.y) (x - center.x)


-- Returns true if the given point is located inside the given circle.
withinCircle : Point -> Circle a -> Bool
withinCircle point {center, radius} =
    distance point center <= radius
