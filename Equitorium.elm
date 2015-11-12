module Equitorium (Model, init, Action, update, view) where

import Graphics.Collage as Collage

import Html exposing (..)

-- MODEL

type alias Model =
    { disks : List (ID, Collage.Form)
    }

type alias ID = Int

enumerate : List a -> List (Int, a)
enumerate = List.indexedMap (,)

init : Model
init =
    let circles = [Collage.circle 5.0, Collage.circle 3.5, Collage.circle 10.0]
        outlineCircles = List.map (Collage.outlined Collage.defaultLine)
    in
        { disks = circles |> outlineCircles |> enumerate }


-- UPDATE

type Action = Rotate ID

update : Action -> Model -> Model
update action model =
  case action of
    Rotate id -> 
        let updateDisk (diskID, diskForm) =
            if diskID >= id
                then (diskID, diskForm)
                else (diskID, diskForm)
        in
            { model | disks <- List.map updateDisk model.disks }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = div [] []