module Main exposing (..)

import Html exposing (Html, form, button, div, text, label, input, legend)
import Html.App as App
import Html.Events exposing (onInput, onDoubleClick)
import Html.Attributes exposing (..)
import Date exposing (Date, Month)
import Task


main =
    App.program { init = model, view = view, update = update, subscriptions = \_ -> Sub.none }


type alias Crop =
    { name : String
    , variety : String
    , germinated : Maybe Date
    }


type Msg
    = Name String
    | Variety String
    | Germinated (Maybe String)
    | Now


type alias Model =
    { crop : Crop
    , germinatedValue : Maybe String
    }


model : ( Model, Cmd Msg )
model =
    ( Model (Crop "" "" Nothing) Nothing, now )


updateCrop : (Crop -> Crop) -> Model -> Model
updateCrop update model =
    { model | crop = update model.crop }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        Name name ->
            ( updateCrop (\crop -> { crop | name = name }) model, Cmd.none )

        Variety variety ->
            ( updateCrop (\crop -> { crop | variety = variety }) model, Cmd.none )

        Germinated germinated ->
            let
                crop =
                    model.crop
            in
                ( { model
                    | crop = { crop | germinated = germinated `Maybe.andThen` (Date.fromString >> Result.toMaybe) }
                    , germinatedValue = germinated
                  }
                , Cmd.none
                )

        Now ->
            ( model, now )


now : Cmd Msg
now =
    Task.perform (always (Germinated Nothing)) (formatDate >> Just >> Germinated) Date.now


toMonthNumber : Month -> Int
toMonthNumber month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


formatNumber : Int -> String
formatNumber number =
    (if number < 10 then
        "0"
     else
        ""
    )
        ++ toString number


formatDate date =
    let
        year =
            date |> Date.year |> toString

        month =
            date |> Date.month |> toMonthNumber |> formatNumber

        day =
            date |> Date.day |> formatNumber
    in
        year ++ "-" ++ month ++ "-" ++ day


defaultDate model =
    model.crop.germinated
        |> Maybe.map formatDate
        |> Maybe.withDefault ""


addCropForm : Model -> Html Msg
addCropForm model =
    Html.form [ class "pure-form pure-form-stacked" ]
        [ legend [] [ text "Add Crop" ]
        , div []
            [ label [] [ text "Name" ]
            , input [ type' "text", placeholder "Lettuce", onInput Name ] []
            ]
        , div []
            [ label [] [ text "Variety" ]
            , input [ type' "text", placeholder "Grand Rapids", onInput Variety ] []
            ]
        , div []
            [ label [] [ text "Date" ]
            , input [ onDoubleClick Now, title "Double click to use today's date", type' "text", value (Maybe.withDefault "" model.germinatedValue), onInput (Just >> Germinated) ] []
            ]
        , button [ type' "submit", class "pure-button pure-button-primary" ] [ text "Create" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "mui-panel mui-container" ]
        [ addCropForm model
        , div [] [ text (toString model) ]
        ]
