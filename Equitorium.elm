module Equitorium (Model, init, Action, update, view) where

import Html exposing (..)

-- MODEL

type alias Model = Int


init : Model
init = 0


-- UPDATE

type Action = Act

update : Action -> Model -> Model
update action model =
  case action of
    Act -> model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = div [] []