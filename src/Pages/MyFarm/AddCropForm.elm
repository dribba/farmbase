module Pages.MyFarm.AddCropForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainMessages exposing (..)
import Models.Main exposing (Model)
import Models.Crop exposing (..)
import Utils.Forms exposing (decodeInt)
import Pages.MyFarm.Types exposing (..)
import Json.Decode as Decode
import Dict


cropTypeInput =
    div
        [ class "form-group" ]
        [ label
            [ class "control-label col-sm-2"
            , for "add-crop-crop-type"
            ]
            [ text "Crop type" ]
        , div [ class "col-xs-10 col-md-4" ]
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
            [ class "control-label col-sm-2"
            , for "add-crop-crop-variety"
            ]
            [ text "Crop variety" ]
        , div [ class "col-xs-10 col-md-4" ]
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
            [ class "control-label col-sm-2"
            , for "add-crop-crop-qty"
            ]
            [ text "Quantity" ]
        , div [ class "col-xs-10 col-md-4" ]
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


mediaOptions : List String
mediaOptions =
    [ "Select media" ] ++ List.sort (Dict.keys mediaTable)


toOption str =
    option [ value str ] [ text str ]


decodeCropMediaUpd =
    Decode.map (cropMediaFromString >> CropMediaUpd) targetValue


cropMediaInput =
    div
        [ class "form-group" ]
        [ label
            [ class "control-label col-sm-2"
            , for "add-crop-crop-media"
            ]
            [ text "Media" ]
        , div [ class "col-xs-10 col-md-4" ]
            [ select
                [ id "add-crop-crop-media"
                , on "change" decodeCropMediaUpd
                , class "form-control"
                ]
                (mediaOptions
                    |> List.map toOption
                )
            ]
        ]


feedingOptions : List String
feedingOptions =
    [ "Select feeding" ] ++ List.sort (Dict.keys feedingTable)


decodeFeedingTypeUpd =
    Decode.map (feedingFromString >> CropFeedingTypeUpd) targetValue


cropFeedingType =
    div
        [ class "form-group" ]
        [ label
            [ class "control-label col-sm-2"
            , for "add-crop-feeding"
            ]
            [ text "Feeding" ]
        , div [ class "col-xs-10 col-md-4" ]
            [ select
                [ id "add-crop-feeding"
                , on "change" decodeFeedingTypeUpd
                , class "form-control"
                ]
                (feedingOptions
                    |> List.map toOption
                )
            ]
        ]


submitButton =
    div
        [ class "form-group" ]
        [ div [ class "col-xs-10 col-sm-offset-2" ]
            [ button
                [ type_ "button"
                , class "btn btn-primary"
                , onClick Submit
                ]
                [ text "Submit" ]
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
        , submitButton
        ]
