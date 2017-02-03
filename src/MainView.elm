module MainView exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Pages.MyFarm.Routing as MyFarmRouting exposing (..)
import Pages.MyFarm.View exposing (..)
import MainMessages exposing (..)
import Models.Main exposing (..)
import Routing exposing (..)


navLink att1 att2 child =
    li att1
        [ a att2 child
        ]


isMyFarmRoute : Route -> Bool
isMyFarmRoute route =
    case route of
        Page (MyFarmPage _) ->
            True

        _ ->
            False


navBar model =
    nav [ class "navbar navbar-default navbar-static-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ a [ href Routing.index, class "navbar-brand" ]
                    [ text "Farmbase" ]
                ]
            , div [ id "navbar", class "navbar-collapse collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    [ navLink
                        [ classList [ ( "active", isMyFarmRoute model.route ) ] ]
                        [ href MyFarmRouting.index ]
                        [ text "My farm" ]
                    , navLink [] [ href "#cropedia" ] [ text "Cropedia" ]
                    , navLink [] [ href "#crop-match" ] [ text "Crop matcher" ]
                    ]
                , div [ class "nav navbar-nav navbar-right" ]
                    [ input [ type_ "text", placeholder "Search" ] []
                    ]
                ]
            ]
        ]


inContainer content =
    div [ class "container" ] content


index =
    inContainer
        [ h1 [] [ text "Index" ]
        , p [] [ text "Soon you'll see some awesome stuff here..." ]
        ]


notFound =
    inContainer [ h1 [] [ text "Not Found" ] ]


error =
    inContainer [ h1 [] [ text "Index" ] ]


page : Model -> PageRoute -> Html MainMessage
page model route =
    case route of
        MyFarmPage myFarmRoute ->
            myFarmView model myFarmRoute


mainView : Model -> Html MainMessage
mainView model =
    div [ id "main" ]
        [ navBar model
        , div [ class "page" ]
            [ case model.route of
                Index ->
                    index

                NotFoundRoute ->
                    notFound

                Routing.Error ->
                    error

                Page route ->
                    page model route
            ]
        ]
