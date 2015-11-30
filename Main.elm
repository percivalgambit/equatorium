import StartApp exposing (start)

import Equatorium exposing (init, update, view, inputs)


app =
    start
        { init = init
        , update = update
        , view = view
        , inputs = inputs
        }

main =
    app.html
