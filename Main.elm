import Disk exposing (init, update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = init { x = 600, y = 200 } 100
    , update = update
    , view = view
    }