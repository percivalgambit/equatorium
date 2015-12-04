module Equatorium (Model, init, Action, update, view, inputs) where

import Disk

import DragAndDrop
import Effects exposing (Effects)
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (on, onClick, targetValue)
import String
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (..)


-- MODEL

type alias Model = 
    { zodiac : Disk.Model
    , deferent : Disk.Model
    , deferentCircle : Disk.Model
    , epicycle : Disk.Model
    , earthDisk : Disk.Model
    , scale : Float
    , dateToSet : 
        { year : Maybe Int
        , month : Maybe Int
        , day : Maybe Int
        }
    , meanEpicyclicAnomaly : Maybe Float
    }

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


init : (Model, Effects Action)
init =
    let
        (zodiac, _) =
            Disk.init { x = 125, y = 125, radius = 110, background = "Zodiac.png" }
        (deferent, deferentFx) =
            Disk.init { x = 125, y = 125, radius = 98, background = "Deferent.png" }
        (deferentCircle, deferentCircleFx) =
            Disk.init { x = 125, y = 120, radius = 90, background = "DeferentCircle.png" }
        (epicycle, epicycleFx) =
            Disk.init { x = 125, y = 70, radius = 35, background = "Epicycle.png" }
        (earthDisk, _) =
            Disk.init { x = 125, y = 120, radius = 14, background = "EarthDisk.png" }
        scale =
            3

        dateToSet =
                { year = Just 1982
                , month = Just 5
                , day = Just 30
                }

        scaleDisk disk =
            { disk | center <- Disk.Point (disk.center.x * scale) (disk.center.y * scale)
            ,        radius <- disk.radius * scale
            }
    in
        ( { zodiac = scaleDisk zodiac
          , deferent = scaleDisk deferent
          , deferentCircle = scaleDisk deferentCircle
          , epicycle = scaleDisk epicycle
          , earthDisk = scaleDisk earthDisk
          , scale = scale

          , dateToSet = dateToSet
          , meanEpicyclicAnomaly = Nothing
          }
        , Effects.batch
            [ Effects.map Deferent deferentFx
            , Effects.map DeferentCircle deferentCircleFx
            , Effects.map Epicycle epicycleFx
            ]
        )


-- Index operator for lists of numbers
(!!) : List number -> Int -> number
xs !! n  = Maybe.withDefault -1 <| List.head (List.drop n xs)
infixl 9 !!


julianDayNumber : Date -> Float
julianDayNumber {year, month, day} =
    let
        centuryYearTable =
            [ 1721057, 1777582, 1794107, 1830632, 1867157, 1903682, 1940207
            , 1976732, 2013257, 2049782, 2086307, 2122832, 2159357, 2195882
            , 2232407, 2268923, 2305447, 2341972, 2378496, 2415020, 2451544
            ]
        centuryTable =
            [ 0,     366,   731,   1096,  1461,  1827,  2192,  2557,  2922,  3288
            , 3653,  4018,  4383,  4749,  5114,  5479,  5844,  6210,  6575,  6940
            , 7305,  7671,  8036,  8401,  8766,  9132,  9497,  9862,  10227, 10593
            , 10958, 11323, 11688, 12054, 12419, 12784, 13149, 13515, 13880, 14245
            , 14610, 14976, 15341, 15706, 16071, 16437, 16802, 17167, 17532, 17898
            , 18263, 18628, 18993, 19359, 19724, 20089, 20454, 20820, 21185, 21550
            , 21915, 22281, 22646, 23011, 23376, 23742, 24107, 24472, 24837, 25203
            , 25568, 25933, 26298, 26664, 27029, 27394, 27759, 28125, 28490, 28855
            , 29220, 29586, 29951, 30316, 30681, 31047, 31412, 31777, 32142, 32508
            , 32873, 33238, 33603, 33969, 34334, 34699, 35064, 35430, 35795, 36160
            ]
        januaryTable =
            [1..31]
        februaryTable =
            [32..60]
        marchTable =
            [60..90]
        aprilTable =
            [91..120]
        mayTable =
            [121..151]
        juneTable =
            [152..181]
        julyTable =
            [182..212]
        augustTable =
            [213..243]
        septemberTable =
            [244..273]
        octoberTable =
            [274..304]
        novemberTable =
            [305..334]
        decemberTable =
            [335..365]
        commonYears =
            [ 15, 17, 18, 19 ]

        monthTable = case month of
            1 -> januaryTable
            2 -> februaryTable
            3 -> marchTable
            4 -> aprilTable
            5 -> mayTable
            6 -> juneTable
            7 -> julyTable
            8 -> augustTable
            9 -> septemberTable
            10 -> octoberTable
            11 -> novemberTable
            12 -> decemberTable

        centuryYear = year // 100
        century = year % 100

        preliminaryDayNumber = (centuryYearTable !! centuryYear)
                               + (centuryTable !! century)
                               + (monthTable !! day)
        dayNumberWithCommonYear =
            if centuryYear `List.member` commonYears && century /= 0 then
                preliminaryDayNumber - 1
            else
                preliminaryDayNumber
        dayNumberWithLeapYear =
            if century % 4 == 0 && month >= 3 then
                if centuryYear `List.member` commonYears && century == 0 then
                    dayNumberWithCommonYear
                else
                    dayNumberWithCommonYear + 1
            else
                dayNumberWithCommonYear
    in
        dayNumberWithLeapYear

-- UPDATE

type Action =
    MouseEvent DragAndDrop.MouseEvent
    | Deferent Disk.Action
    | DeferentCircle Disk.Action
    | Epicycle Disk.Action
    | Year String
    | Month String
    | Day String
    | SetDate
    | None


update : Action -> Model -> (Model, Effects Action)
update action model =
    let
        sameModel =
            ( model
            , Effects.none
            )
        dateToSet = model.dateToSet
    in
        case action of
            MouseEvent mouseEvent ->
                let
                    epicycleAction =
                        Maybe.map Epicycle <| Disk.mouseEventToDiskAction mouseEvent model.epicycle
                    deferentAction =
                        Maybe.map Deferent <| Disk.mouseEventToDiskAction mouseEvent model.deferent
                    deferentCircleAction =
                        Maybe.map DeferentCircle <| Disk.mouseEventToDiskAction mouseEvent model.deferentCircle
                in
                    case Maybe.oneOf [ epicycleAction, deferentCircleAction, deferentAction ] of
                        Just act ->
                            update act model
                        Nothing ->
                            sameModel
            Deferent act ->
                let
                    (newDeferent, deferentFx) = Disk.update act model.deferent
                    (newDeferentCircle, deferentCircleFx) = Disk.update act model.deferentCircle
                    (newEpicycle, epicycleFx) = Disk.update act model.epicycle
                    (newEarthDisk, _) = Disk.update act model.earthDisk
                in
                    case act of
                        Disk.Rotate _ _ ->
                            ( { model | deferent <- newDeferent
                                      , deferentCircle <- newDeferentCircle
                                      , epicycle <- newEpicycle
                                      , earthDisk <- newEarthDisk
                                      , meanEpicyclicAnomaly <- Nothing }
                            , Effects.batch
                                [ Effects.map Deferent deferentFx
                                , Effects.map DeferentCircle deferentCircleFx
                                , Effects.map Epicycle epicycleFx
                                ]
                            )
                        _ ->
                            ( { model | deferent <- newDeferent
                                      , meanEpicyclicAnomaly <- Nothing }
                            , Effects.map Deferent deferentFx
                            )
            DeferentCircle act ->
                let
                    (newDeferentCircle, deferentCircleFx) = Disk.update act model.deferentCircle
                    (newEpicycle, epicycleFx) = Disk.update act model.epicycle
                in
                    case act of
                        Disk.Rotate _ _ ->
                            ( { model | deferentCircle <- newDeferentCircle
                                      , epicycle <- newEpicycle
                                      , meanEpicyclicAnomaly <- Nothing }
                            , Effects.batch
                                [ Effects.map DeferentCircle deferentCircleFx
                                , Effects.map Epicycle epicycleFx
                                ]
                            )
                        _ ->
                            ( { model | deferentCircle <- newDeferentCircle
                                      , meanEpicyclicAnomaly <- Nothing }
                            , Effects.map DeferentCircle deferentCircleFx
                            )
            Epicycle act ->
                let
                    (newEpicycle, epicycleFx) = Disk.update act model.epicycle
                in
                    ( { model | epicycle <- newEpicycle
                              , meanEpicyclicAnomaly <- Nothing }
                    , Effects.map Epicycle epicycleFx
                    )
            Year year ->
                ( { model | dateToSet <-
                    { dateToSet | year <- Result.toMaybe <| String.toInt year } 
                  }
                , Effects.none
                )
            Month month ->
                ( { model | dateToSet <-
                    { dateToSet | month <- Result.toMaybe <| String.toInt month } 
                  }
                , Effects.none
                )
            Day day ->
                ( { model | dateToSet <-
                    { dateToSet | day <- Result.toMaybe <| String.toInt day } 
                  }
                , Effects.none
                )
            SetDate ->
                case model.dateToSet.year of
                    Just year ->
                        case model.dateToSet.month of
                            Just month ->
                                case model.dateToSet.day of
                                    Just day ->
                                        ( dateToEquatoriumPosition <| Date year month day
                                        , Effects.none
                                        )
                                    Nothing ->
                                        sameModel
                            Nothing ->
                                sameModel
                    Nothing ->
                        sameModel
            None ->
                sameModel


dateToEquatoriumPosition : Date -> Model
dateToEquatoriumPosition date =
    let
        epoch =
            2415020
        meanMotionInLongitude =
            0.52407116
        meanMotionInEpicyclicAnomaly =
            0.46157618
        rateOfPrecession =
            1.807
        apogeeLongitudeAtEpoch =
            148 + (37/60)
        meanLongitudeAtEpoch =
            293 + (33/60)
        meanEpicyclicAnomalyAtEpoch =
            346 + (9/60)

        julianDaysSinceEpoch =
            julianDayNumber date - epoch
        julianCenturiesSinceEpoch =
            julianDaysSinceEpoch / 36525
        apogeeLongitude =
            apogeeLongitudeAtEpoch + rateOfPrecession * julianCenturiesSinceEpoch
        meanLongitude =
            meanLongitudeAtEpoch + meanMotionInLongitude * julianDaysSinceEpoch
        meanEpicyclicAnomaly =
            meanEpicyclicAnomalyAtEpoch + meanMotionInEpicyclicAnomaly * julianDaysSinceEpoch

        (initialModel, _) =
            init
        (setDeferentApogeeModel, _) =
            update
                (Deferent <| Disk.Rotate initialModel.deferent.center 
                                         (degrees <| 90 - apogeeLongitude))
                initialModel
        (setMeanMotionModel, _) =
            let
                earthDisk =
                    { center = setDeferentApogeeModel.zodiac.center
                    , radius = 10 * setDeferentApogeeModel.scale
                    , angle = setDeferentApogeeModel.earthDisk.angle
                    }
                pointOnEarthDisk =
                    Disk.getAnglePosition earthDisk
                deferentCircle =
                    { center = pointOnEarthDisk
                    , radius = 25 * setDeferentApogeeModel.scale
                    , angle = degrees <| 90 - meanLongitude
                    }
                pointOnDeferentCircle =
                    Disk.getAnglePosition deferentCircle
                deferentCircleAngle =
                    -pi/2 - atan2
                        (pointOnDeferentCircle.y - setDeferentApogeeModel.deferentCircle.center.y)
                        (pointOnDeferentCircle.x - setDeferentApogeeModel.deferentCircle.center.x)
            in
                update
                    (DeferentCircle <| Disk.Rotate setDeferentApogeeModel.deferentCircle.center
                                                   deferentCircleAngle)
                    setDeferentApogeeModel
    in
        { setMeanMotionModel | dateToSet <-
                                { year = Just date.year
                                , month = Just date.month
                                , day = Just date.day
                                }
                             , meanEpicyclicAnomaly <- Just meanEpicyclicAnomaly }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address {zodiac, deferent, deferentCircle, epicycle, earthDisk, scale,
              dateToSet, meanEpicyclicAnomaly} =
    let
        noAction = Signal.forwardTo address <| always None
        maybeToString maybe =
            Maybe.withDefault "" <| Maybe.map toString maybe

        dateField =
            div
                []
                [ text "Select a date:"
                , div
                    []
                    [ text "Year"
                    , input
                        [ value <| maybeToString dateToSet.year
                        , on "input" targetValue (Signal.message address << Year)
                        ]
                        []
                    ]
                , div
                    []
                    [ text "Month"
                    , input
                        [ value <| maybeToString dateToSet.month
                        , on "input" targetValue (Signal.message address << Month)
                        ]
                        []
                    ]
                , div
                    []
                    [ text "Day"
                    , input
                        [ value <| maybeToString dateToSet.day
                        , on "input" targetValue (Signal.message address << Day)
                        ]
                        []
                    ]
                , button
                    [ onClick address SetDate ]
                    [ text "Set Date" ]
                ]

        earth =
            case meanEpicyclicAnomaly of
                Just _ ->
                    circle
                        [ cx <| toString <| zodiac.center.x
                        , cy <| toString <| zodiac.center.y
                        , r "5"
                        , fill "#0000FF"
                        ]
                        []
                Nothing ->
                    g [] []
        mars =
            case meanEpicyclicAnomaly of
                Just marsAngle ->
                    let
                        marsDisk =
                            { center = epicycle.center
                            , radius = epicycle.radius
                            , angle = epicycle.angle - marsAngle
                            }
                        marsPosition =
                            Disk.getAnglePosition marsDisk
                    in
                        circle
                            [ cx <| toString <| marsPosition.x
                            , cy <| toString <| marsPosition.y
                            , r "5"
                            , fill "#FF0000"
                            ]
                            []
                Nothing ->
                    g [] []
        marsLongitudeText =
            case meanEpicyclicAnomaly of
                Just marsAngle ->
                    let
                        marsDisk =
                            { center = epicycle.center
                            , radius = epicycle.radius
                            , angle = epicycle.angle - marsAngle
                            }
                        marsPosition =
                            Disk.getAnglePosition marsDisk
                        marsLongitudeRadians =
                            -(atan2 (marsPosition.y - zodiac.center.y)
                                    (marsPosition.x - zodiac.center.x))
                        marsLongitude =
                            marsLongitudeRadians * 180/pi
                        marsLongitudePositive =
                            if marsLongitude < 0 then
                                marsLongitude + 360
                            else
                                marsLongitude

                    in
                        text <| "The longitude of mars is "
                                ++ toString marsLongitudePositive
                Nothing ->
                    text ""

        equatorium =
            svg
                [ width <| toString <| zodiac.center.x + zodiac.radius
                , height <| toString <| zodiac.center.y + zodiac.radius
                ] 
                <| List.concat
                    [ Disk.view noAction zodiac
                    , Disk.view (Signal.forwardTo address Deferent) deferent
                    , Disk.view (Signal.forwardTo address DeferentCircle) deferentCircle
                    , Disk.view (Signal.forwardTo address Epicycle) epicycle
                    , Disk.view noAction earthDisk
                    , [ earth, mars ]
                    ]
    in
        div
            []
            [ equatorium, dateField, marsLongitudeText ]


-- INPUTS

inputs : List (Signal Action)
inputs = [ Signal.map MouseEvent DragAndDrop.mouseEvents ]