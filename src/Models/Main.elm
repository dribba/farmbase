module Models.Main exposing (..)

import Models.Crop exposing (..)
import Routing
import Http exposing (Error, Request)


type AsyncData b a
    = Empty
    | Loading
    | Success a
    | Error b


type alias Model =
    { cropForm : CropForm
    , crops : AsyncData Error (List Crop)
    , route : Routing.Route
    }
