module Models.Crop exposing (..)

import Date exposing (Date)
import Json.Decode as Decode
import Json.Encode as Encode
import Dict
import Utils.DateUtils exposing (formatDate, decodeDate)


type CropMedia
    = Hydroponics
    | CocoOrPeat
    | Aeroponics
    | Soil


type FeedingType
    = Fertilizer
    | Nutrients
    | Compost


type alias Crop =
    { type_ : String
    , variety : String
    , quantity : Int
    , media : Maybe CropMedia
    , feeding : Maybe FeedingType
    , started : Maybe Date
    , created : Maybe Date
    , id : String
    }


type alias CropForm =
    { type_ : String
    , variety : String
    , quantity : Int
    , media : Maybe CropMedia
    , feeding : Maybe FeedingType
    , started : Maybe Date
    , id : Maybe String
    }


emptyCropForm =
    CropForm "" "" 1 Nothing Nothing Nothing Nothing



-- Encoders:


decodeCrop : Decode.Decoder Crop
decodeCrop =
    Decode.map8 Crop
        (Decode.field "type" Decode.string)
        (Decode.field "variety" Decode.string)
        (Decode.field "quantity" Decode.int)
        (Decode.field "media" decodeCropMedia)
        (Decode.field "feeding" decodeFeeding)
        (Decode.field "started" decodeDate)
        (Decode.field "created" decodeDate)
        (Decode.field "_id" Decode.string)


maybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybe encoder value =
    case value of
        Just a ->
            encoder a

        Nothing ->
            Encode.null


encodeCropForm : CropForm -> Encode.Value
encodeCropForm cropForm =
    Encode.object
        [ ( "type", Encode.string cropForm.type_ )
        , ( "variety", Encode.string cropForm.variety )
        , ( "quantity", Encode.int cropForm.quantity )
        , ( "media", maybe (toString >> Encode.string) cropForm.media )
        , ( "feeding", maybe (toString >> Encode.string) cropForm.feeding )
        , ( "started", maybe (Encode.float << Date.toTime) cropForm.started )
        ]


feedingTable =
    Dict.fromList
        [ ( "Fertilizer", Fertilizer )
        , ( "Nutrients", Nutrients )
        , ( "Compost", Compost )
        ]


feedingFromString : String -> Maybe FeedingType
feedingFromString str =
    Dict.get str feedingTable


decodeFeeding : Decode.Decoder (Maybe FeedingType)
decodeFeeding =
    Decode.map feedingFromString Decode.string


cropMediaFromString : String -> Maybe CropMedia
cropMediaFromString str =
    Dict.get str mediaTable


decodeCropMedia : Decode.Decoder (Maybe CropMedia)
decodeCropMedia =
    Decode.map cropMediaFromString Decode.string


mediaTable =
    Dict.fromList
        [ ( "Hydroponics", Hydroponics )
        , ( "Coco/Peat", CocoOrPeat )
        , ( "Aeroponics", Aeroponics )
        , ( "Soil", Soil )
        ]
