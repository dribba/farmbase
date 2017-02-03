module Pages.MyFarm.Types exposing (..)


type CropMedia
    = Hydroponics
    | CocoOrPeat
    | Aeroponics
    | Soil


type FeedingType
    = Fertilizer
    | Nutrients
    | Compost


type AddCropFormMessage
    = CropTypeUpd String
    | CropVarietyUpd String
    | CropQtyUpd Int
    | CropMediaUpd (Maybe CropMedia)
    | CropFeedingTypeUpd (Maybe FeedingType)
    | Submit


type MyFarmMessage
    = AddCropFormMessage AddCropFormMessage
