module Disk where

import Graphics.Collage as Collage

-- MODEL

type alias Model =
    { disk : Collage.Form }

init : Float -> Model
init = Collage.circle >> Collage.outlined Collage.defaultLine


-- UPDATE

type Action = Rotate

update : Action -> Model -> Model
update action model =
  case action of
    Rotate ->
        model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = div [] []