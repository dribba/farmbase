module Pages.MyFarm.Update exposing (..)

import MainMessages exposing (..)
import Pages.MyFarm.Types exposing (..)
import Models.Main exposing (Model)


update : MyFarmMessage -> Model -> ( Model, Cmd MainMessage )
update msg model =
    case msg of
        _ ->
            model ! []
