module Pages.MyFarm.AddCropForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainMessages exposing (..)
import Models.Main exposing (Model)
import Utils.Forms exposing (decodeInt)
import Pages.MyFarm.Types exposing (..)
import Json.Decode as Decode
import Dict


cropTypeInput =
    div
        [ class "form-group" ]
        [ label
            [ for "add-crop-crop-type" ]
            [ text "Crop type" ]
        , div [ class "col-xs-12" ]
            [ input
                [ type_ "text"
                , id "add-crop-crop-type"
                , class "form-control"
                , placeholder "Lettuce"
                , onInput CropTypeUpd
                ]
                []
            ]
        ]


cropVarietyInput =
    div
        [ class "form-group" ]
        [ label
            [ for "add-crop-crop-variety" ]
            [ text "Crop type" ]
        , div [ class "col-xs-12" ]
            [ input
                [ type_ "text"
                , id "add-crop-crop-variety"
                , class "form-control"
                , placeholder "Grand Rapids"
                , onInput CropVarietyUpd
                ]
                []
            ]
        ]


cropQuantityInput =
    div
        [ class "form-group" ]
        [ label
            [ for "add-crop-crop-qty" ]
            [ text "Crop type" ]
        , div [ class "col-xs-12" ]
            [ input
                [ type_ "number"
                , id "add-crop-crop-qty"
                , class "form-control"
                , defaultValue "1"
                , on "input" (decodeInt |> Decode.map CropQtyUpd)
                ]
                []
            ]
        ]


mediaTable =
    Dict.fromList
        [ ( "Hydroponics", Hydroponics )
        , ( "Coco/Peat", CocoOrPeat )
        , ( "Aeroponics", Aeroponics )
        , ( "Soil", Soil )
        ]


mediaOptions : List String
mediaOptions =
    [ "Select media" ] ++ List.sort (Dict.keys mediaTable)


toOption str =
    option [ value str ] [ text str ]


cropMediaFromString : String -> Maybe CropMedia
cropMediaFromString str =
    Dict.get str mediaTable


decodeCropMedia : Decode.Decoder AddCropFormMessage
decodeCropMedia =
    Decode.map (cropMediaFromString >> CropMediaUpd) targetValue


cropMediaInput =
    div
        [ class "form-group" ]
        [ label
            [ for "add-crop-crop-media" ]
            [ text "Media" ]
        , div [ class "col-xs-12" ]
            [ select
                [ id "add-crop-crop-media"
                , on "change" decodeCropMedia
                , class "form-control"
                ]
                (mediaOptions
                    |> List.map toOption
                )
            ]
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


decodeFeeding : Decode.Decoder AddCropFormMessage
decodeFeeding =
    Decode.map (feedingFromString >> CropFeedingTypeUpd) targetValue


feedingOptions : List String
feedingOptions =
    [ "Select feeding" ] ++ List.sort (Dict.keys feedingTable)


cropFeedingType =
    div
        [ class "form-group" ]
        [ label
            [ for "add-crop-feeding" ]
            [ text "Feeding" ]
        , div [ class "col-xs-12" ]
            [ select
                [ id "add-crop-feeding"
                , on "change" decodeFeeding
                , class "form-control"
                ]
                (feedingOptions
                    |> List.map toOption
                )
            ]
        ]


myFarmAddCropForm : Model -> Html AddCropFormMessage
myFarmAddCropForm model =
    Html.form [ class "form-horizontal" ]
        [ cropTypeInput
        , cropVarietyInput
        , cropQuantityInput
        , cropMediaInput
        , cropFeedingType
        , button [ onClick Submit ] [ text "Submit" ]
        ]
