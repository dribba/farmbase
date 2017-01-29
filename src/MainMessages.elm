module MainMessages exposing (..)

import Date exposing (Date)
import Models.Crop exposing (Crop)
import Http exposing (Error, Request)
import Navigation exposing (Location)


type MainMessage
    = Name String
    | Variety String
    | Planted (Maybe Date)
    | PlantedValue (Maybe String)
    | Now
    | RefreshCrops
    | ReceiveCrops (List Crop)
    | CropsError Error
    | SaveCropForm
    | CropSaved Crop
    | CropSaveError Error
    | RemoveCrop Crop
    | CropRemoved
    | CropRemoveError
    | EditCrop Crop
    | ResetCropForm
    | OnLocationChange Location
