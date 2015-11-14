import Disk exposing (init, update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = init 5.0
    , update = update
    , view = view
    }