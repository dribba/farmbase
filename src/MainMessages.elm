module MainMessages exposing (..)

import Date exposing (Date)
import Models.Crop exposing (Crop)
import Http exposing (Error, Request)
import Navigation exposing (Location)
import Pages.MyFarm.Types exposing (MyFarmMessage)


type PageMessage
    = MyFarmMessage MyFarmMessage


type MainMessage
    = Now
    | RefreshCrops
    | ReceiveCrops (List Crop)
      -- | CropSaved Crop
      -- | SaveCropForm
      -- | CropSaveError Error
      -- | RemoveCrop Crop
      -- | CropRemoved
      -- | CropRemoveError
      -- | EditCrop Crop
      -- | ResetCropForm
    | PageMessage PageMessage
    | OnLocationChange Location
