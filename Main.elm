-- Mostly coppied from the evancz/start-app elm package

import Equatorium exposing (init, update, view, inputs)

import Html exposing (Html)


type alias Config model action =
    { init : model
    , update : action -> model -> model
    , view : Signal.Address action -> model -> Html
    , inputs : List (Signal action)
    }


type alias App model =
    { html : Signal Html
    , model : Signal model
    }


start : Config model action -> App model
start config =
    let
        singleton action = [ action ]

        -- messages : Signal.Mailbox (List action)
        messages =
            Signal.mailbox []

        -- address : Signal.Address action
        address =
            Signal.forwardTo messages.address singleton

        -- updateStep : action -> (model, Effects action) -> (model, Effects action)
        updateStep action oldModel =
            config.update action oldModel

        -- update : List action -> (model, Effects action) -> (model, Effects action)
        update actions model =
            List.foldl updateStep model actions

        -- inputs : Signal (List action)
        inputs =
            Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) config.inputs)

        model =
            Signal.foldp update config.init inputs
    in
        { html = Signal.map (config.view address) model
        , model = model
        }


app =
    start
        { init = init
        , update = update
        , view = view
        , inputs = inputs
        }


main =
    app.html
