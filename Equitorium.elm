module Counter (Model, init, Action, update, view) where

import Html exposing (..)

-- MODEL

type alias Model = Int


-- UPDATE

type Action = Act

update : Action -> Model -> Model
update action model =
  case action of
    Act -> return model


-- VIEW

view : Model -> Html
view = div [] []