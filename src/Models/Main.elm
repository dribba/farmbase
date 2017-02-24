module Models.Main exposing (..)

import Models.Crop exposing (..)
import Pages.MyFarm.Types exposing (..)
import Routing
import Http exposing (Error, Request)
import Utils.AsyncData exposing (AsyncData(..))


type alias PageData =
    { myFarm : MyFarmModel }


emptyPageData : PageData
emptyPageData =
    { myFarm = emptyMyFarmModel }


type alias Model =
    { pageData : PageData
    , crops : AsyncData Error (List Crop)
    , route : Routing.Route
    }


emptyModel : Routing.Route -> Model
emptyModel route =
    { pageData = emptyPageData
    , crops = Empty
    , route = route
    }
