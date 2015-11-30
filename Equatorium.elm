module Equatorium (Model, init, Action, update, view, inputs) where

import Disk

import Date exposing (Date)
import DragAndDrop
import Effects exposing (Effects)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


-- MODEL

type alias Model = 
    { zodiac : Disk.Model
    , deferent : Disk.Model
    , deferentCircle : Disk.Model
    , epicycle : Disk.Model
    , earth : Disk.Model
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
        (earth, _) =
            Disk.init { x = 125, y = 120, radius = 14, background = "Earth.png" }
        scale =
            3

        scaleDisk disk =
            { disk | center <- Disk.Point (disk.center.x * scale) (disk.center.y * scale)
            ,        radius <- disk.radius * scale
            }
    in
        ( { zodiac = scaleDisk zodiac
          , deferent = scaleDisk deferent
          , deferentCircle = scaleDisk deferentCircle
          , epicycle = scaleDisk epicycle
          , earth = scaleDisk earth
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
julianDayNumber date =
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

        dateCenturyYear = Date.year date // 100
        dateCentury = Date.year date % 100
        dateMonthTable = case Date.month date of
            Date.Jan -> januaryTable
            Date.Feb -> februaryTable
            Date.Mar -> marchTable
            Date.Apr -> aprilTable
            Date.May -> mayTable
            Date.Jun -> juneTable
            Date.Jul -> julyTable
            Date.Aug -> augustTable
            Date.Sep -> septemberTable
            Date.Oct -> octoberTable
            Date.Nov -> novemberTable
            Date.Dec -> decemberTable
        dateDay = Date.day date

        preliminaryDayNumber = (centuryYearTable !! dateCenturyYear)
                               + (centuryTable !! dateCentury)
                               + (dateMonthTable !! dateDay)
        dayNumberWithCommonYear =
            if dateCenturyYear `List.member` commonYears && dateCentury /= 0 then
                preliminaryDayNumber - 1
            else
                preliminaryDayNumber
        dayNumberWithLeapYear =
            if dateCentury % 4 == 0 && dateMonthTable !! 1 >= 60 then
                if dateCenturyYear `List.member` commonYears && dateCentury == 0 then
                    dayNumberWithCommonYear
                else
                    dayNumberWithCommonYear + 1
            else
                dayNumberWithCommonYear
    in
        dayNumberWithLeapYear + toFloat (12 - Date.hour date) / 24

-- UPDATE

type Action =
    MouseEvent DragAndDrop.MouseEvent
    | Deferent Disk.Action
    | DeferentCircle Disk.Action
    | Epicycle Disk.Action
    | None


update : Action -> Model -> (Model, Effects Action)
update action model =
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
                    ( model
                    , Effects.none
                    )
    Deferent act ->
        let
            (newDeferent, deferentFx) = Disk.update act model.deferent
            (newDeferentCircle, deferentCircleFx) = Disk.update act model.deferentCircle
            (newEpicycle, epicycleFx) = Disk.update act model.epicycle
            (newEarth, _) = Disk.update act model.earth
        in
            case act of
                Disk.Rotate _ _ ->
                    ( { model | deferent <- newDeferent
                              , deferentCircle <- newDeferentCircle
                              , epicycle <- newEpicycle
                              , earth <- newEarth }
                    , Effects.batch
                        [ Effects.map Deferent deferentFx
                        , Effects.map DeferentCircle deferentCircleFx
                        , Effects.map Epicycle epicycleFx
                        ]
                    )
                _ ->
                    ( { model | deferent <- newDeferent }
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
                              , epicycle <- newEpicycle }
                    , Effects.batch
                        [ Effects.map DeferentCircle deferentCircleFx
                        , Effects.map Epicycle epicycleFx
                        ]
                    )
                _ ->
                    ( { model | deferentCircle <- newDeferentCircle }
                    , Effects.map DeferentCircle deferentCircleFx
                    )
    Epicycle act ->
        let
            (newEpicycle, epicycleFx) = Disk.update act model.epicycle
        in
            ( { model | epicycle <- newEpicycle }
            , Effects.map Epicycle epicycleFx
            )
    None ->
        ( model
        , Effects.none
        )


dateToEquatoriumPosition : Date -> Model
dateToEquatoriumPosition date =
    let
        (initialModel, _) =
            init
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

    in
        initialModel

-- VIEW

view : Signal.Address Action -> Model -> Svg
view address {zodiac, deferent, deferentCircle, epicycle, earth} =
    let
        noAction = Signal.forwardTo address <| always None
    in
        svg
            [ width <| toString <| zodiac.center.x + zodiac.radius
            , height <| toString <| zodiac.center.y + zodiac.radius
            ] 
            <| List.concat
                [ Disk.view noAction zodiac
                , Disk.view (Signal.forwardTo address Deferent) deferent
                , Disk.view (Signal.forwardTo address DeferentCircle) deferentCircle
                , Disk.view (Signal.forwardTo address Epicycle) epicycle
                , Disk.view noAction earth
                ]


-- INPUTS

inputs : List (Signal Action)
inputs = [ Signal.map MouseEvent DragAndDrop.mouseEvents ]