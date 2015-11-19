import StartApp.Simple exposing (start)

import Disk
import Equitorium exposing (init, update, view)


main =
  start
    { model = init
        { deferent = Disk.init { x = 250, y = 250, radius = 125 }
        , epicycle = Disk.init { x = 250, y = 200, radius = 50 }
        }
    , update = update
    , view = view
    }