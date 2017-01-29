module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Pages.MyFarm.Routing as MyFarmRouting


type PageRoute
    = MyFarmPage MyFarmRouting.MyFarmRoute


type Route
    = Index
    | NotFoundRoute
    | Error
    | Page PageRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Index top
        , map (Page << MyFarmPage) MyFarmRouting.matchers
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


index =
    "#"
