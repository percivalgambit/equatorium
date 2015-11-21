module Equitorium (Model, init, Action, update, view, inputs) where

import Disk

import DragAndDrop
import Effects exposing (Effects)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


-- MODEL

type alias Model = 
    { deferent : Disk.Model
    , epicycle : Disk.Model
    }


init : (Model, Effects Action)
init =
    let
        (deferent, deferentFx) = Disk.init { x = 250, y = 250, radius = 125 }
        (epicycle, epicycleFx) = Disk.init { x = 250, y = 200, radius = 50 }
    in
        ( Model deferent epicycle
        , Effects.batch
            [ Effects.map Deferent deferentFx
            , Effects.map Epicycle epicycleFx
            ]
        )


-- UPDATE

type Action
    = Deferent Disk.Action
    | Epicycle Disk.Action


update : Action -> Model -> (Model, Effects Action)
update action {deferent, epicycle} =
  case action of
    Deferent act ->
        let
            (newDeferent, deferentFx) = Disk.update act deferent
            (newEpicycle, epicycleFx) = Disk.update act epicycle
        in
            ( Model newDeferent newEpicycle
            , Effects.batch
                [ Effects.map Deferent deferentFx
                , Effects.map Epicycle epicycleFx
                ]
            )
    Epicycle act ->
        let
            (newEpicycle, epicycleFx) = Disk.update act epicycle
        in
            ( Model deferent newEpicycle
            , Effects.map Epicycle epicycleFx
            )


-- VIEW

view : Signal.Address Action -> Model -> Svg
view address {deferent, epicycle} = 
    svg
        [ width << toString <| deferent.center.x + deferent.radius * 1.1
        , height << toString <| deferent.center.y + deferent.radius * 1.1
        ]
        [ Disk.view (Signal.forwardTo address Deferent) deferent
        , Disk.view (Signal.forwardTo address Epicycle) epicycle
        ]


-- INPUTS

inputs : List (Signal Action)
inputs = []