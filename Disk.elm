module Disk where

import Graphics.Collage as Collage

import Html exposing (..)

-- MODEL

type alias Model =
    { disk : Collage.Form }

init : Float -> Model
init radius = let circle = Collage.circle radius
              in { disk = Collage.outlined Collage.defaultLine circle }


-- UPDATE

type Action = Rotate

update : Action -> Model -> Model
update action model =
  case action of
    Rotate ->
        model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = let element = Collage.collage 0 0 [model.disk]
                     in div [] [fromElement element]