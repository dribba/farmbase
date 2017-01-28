module MainView exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Pages.MyFarm.Routing as MyFarmRouting


navLink att1 att2 child =
    li att1
        [ a att2 child
        ]


navBar =
    nav [ class "navbar navbar-default navbar-static-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ a [ href "#index", class "navbar-brand" ]
                    [ text "Farmbase" ]
                ]
            , div [ id "navbar", class "navbar-collapse collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    [ navLink [] [ href MyFarmRouting.index ] [ text "My farm" ]
                    , navLink [] [ href "#cropedia" ] [ text "Cropedia" ]
                    , navLink [] [ href "#crop-match" ] [ text "Crop matcher" ]
                    ]
                , div [ class "nav navbar-nav navbar-right" ]
                    [ input [ type_ "text", placeholder "Search" ] []
                    ]
                ]
            ]
        ]



-- main =
--     div [ id "main" ]
