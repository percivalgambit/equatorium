module Equitorium (Model, init, Action, update, view) where

import Disk

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)

-- MODEL

type alias Model = 
    { deferent : Disk.Model
    , epicycle : Disk.Model
    }

init : Model -> Model
init = identity


-- UPDATE

type Action
    = Deferent Disk.Action
    | Epicycle Disk.Action

update : Action -> Model -> Model
update action {deferent, epicycle} =
  case action of
    Deferent act ->
        let
            newDeferent = Disk.update act deferent
            newEpicycle = Disk.update act epicycle
        in
            Model newDeferent newEpicycle
    Epicycle act ->
        let
            newEpicycle = Disk.update act epicycle
        in
            Model deferent newEpicycle

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