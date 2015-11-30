module Equatorium (Model, init, Action, update, view, inputs) where

import Disk

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