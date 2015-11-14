import Disk exposing (init, update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = init (75, 100) 50
    , update = update
    , view = view
    }