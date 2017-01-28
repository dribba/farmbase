module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Pages.MyFarm.Routing as MyFarmRouting


type Route
    = Index
    | NotFoundRoute
    | Error
    | PageRoute MyFarmRouting.MyFarmRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Index top
        , map PageRoute MyFarmRouting.matchers
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
