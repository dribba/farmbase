module Pages.MyFarm.View exposing (..)

import Pages.MyFarm.Routing exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import MainMessages exposing (MainMessage)


myFarmView : MyFarmRoute -> Html MainMessage
myFarmView route =
    case route of
        MyFarmIndex ->
            div [ class "container" ]
                [ h1 [] [ text "My Farm" ] ]

        _ ->
            div [ class "container" ]
                [ h1 [] [ text "Dead end" ] ]
