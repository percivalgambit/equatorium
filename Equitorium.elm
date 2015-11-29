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
        (deferent, deferentFx) =
            Disk.init { x = 250, y = 250, radius = 125, background = "Disk3.png" }
        (epicycle, epicycleFx) =
            Disk.init { x = 250, y = 200, radius = 50, background = "Disk2.png" }
    in
        ( Model deferent epicycle
        , Effects.none
        )


-- UPDATE

type Action =
    MouseEvent DragAndDrop.MouseEvent | Deferent Disk.Action | Epicycle Disk.Action


update : Action -> Model -> (Model, Effects Action)
update action {deferent, epicycle} =
  case action of
    MouseEvent mouseEvent ->
        let
            epicycleAction =
                Maybe.map Epicycle <| Disk.mouseEventToDiskAction mouseEvent epicycle
            deferentAction =
                Maybe.map Deferent <| Disk.mouseEventToDiskAction mouseEvent deferent
        in
            case Maybe.oneOf [ epicycleAction, deferentAction ] of
                Just act ->
                    update act (Model deferent epicycle)
                Nothing ->
                    ( Model deferent epicycle
                    , Effects.none
                    )
    Deferent act ->
        let
            (newDeferent, deferentFx) = Disk.update act deferent
            (newEpicycle, epicycleFx) = Disk.update act epicycle
        in
            case act of
                Disk.Rotate _ _ ->
                    ( Model newDeferent newEpicycle
                    , Effects.none
                    )
                _ ->
                    ( Model newDeferent epicycle
                    , Effects.none
                    )
    Epicycle act ->
        let
            (newEpicycle, epicycleFx) = Disk.update act epicycle
        in
            ( Model deferent newEpicycle
            , Effects.none
            )


-- VIEW

view : Signal.Address Action -> Model -> Svg
view address {deferent, epicycle} = 
    svg
        [ width << toString <| deferent.center.x + deferent.radius * 1.1
        , height << toString <| deferent.center.y + deferent.radius * 1.1
        ]
        (Disk.view (Signal.forwardTo address Deferent) deferent
        ++ Disk.view (Signal.forwardTo address Epicycle) epicycle)


-- INPUTS

inputs : List (Signal Action)
inputs = [ Signal.map MouseEvent DragAndDrop.mouseEvents ]