module Pages.MyFarm.Types exposing (..)

import Models.Crop exposing (..)
import Http


type alias MyFarmModel =
    { addCropForm : CropForm }


emptyMyFarmModel =
    { addCropForm = emptyCropForm }


type AddCropFormMessage
    = CropTypeUpd String
    | CropVarietyUpd String
    | CropQtyUpd Int
    | CropMediaUpd (Maybe CropMedia)
    | CropFeedingTypeUpd (Maybe FeedingType)
    | AddCropResponse (Result Http.Error Crop)
    | Submit


type MyFarmMessage
    = AddCropFormMessage AddCropFormMessage
