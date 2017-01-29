module Models.Crop exposing (..)

import Date exposing (Date)


type alias Crop =
    { id : String
    , name : String
    , variety : String
    , planted : Maybe Date
    }


type alias CropForm =
    { name : String
    , variety : String
    , planted : Maybe Date
    , plantedValue : Maybe String
    , id : Maybe String
    }
