module Pages.MyFarm.View exposing (..)

import Pages.MyFarm.Routing exposing (..)
import Pages.MyFarm.Types exposing (..)
import Pages.MyFarm.AddCropForm exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import MainMessages exposing (..)
import Models.Main exposing (Model)


myFarmView : Model -> MyFarmRoute -> Html MainMessage
myFarmView model route =
    case route of
        MyFarmIndex ->
            myFarmIndex model

        MyFarmAdd ->
            myFarmAdd model

        _ ->
            div [ class "container" ]
                [ h1 [] [ text "Dead end" ] ]


myFarmIndex : Model -> Html MainMessage
myFarmIndex model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-xs-12 col-md-8" ] [ h2 [] [ text "My Farm" ] ]
            , div [ class "col-xs-6 col-md-4" ] [ a [ class "btn btn-primary", href add ] [ text "Add" ] ]
            ]
        ]


myFarmAdd : Model -> Html MainMessage
myFarmAdd model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-xs-12" ] [ h5 [] [ text "Add crop:" ] ]
            , div [ class "col-xs-12" ]
                [ div []
                    [ Html.map (AddCropFormMessage >> MyFarmMessage >> PageMessage) (myFarmAddCropForm model)
                    ]
                ]
            ]
        ]
