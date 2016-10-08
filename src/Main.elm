module Main exposing (..)

import Html exposing (Html, form, button, div, text, label, input, legend, table, th, tr, td, thead, tbody)
import Html.App as App
import Html.Events exposing (onInput, onDoubleClick)
import Html.Attributes exposing (..)
import Utils.DateUtils exposing (formatDate)
import Date exposing (Date, Month)
import Task
import Platform.Cmd exposing (Cmd, (!))


main =
    App.program { init = model, view = view, update = update, subscriptions = \_ -> Sub.none }


type alias NewCrop =
    { name : String
    , variety : String
    , planted : Maybe Date
    , plantedValue : Maybe String
    }


type Msg
    = Name String
    | Variety String
    | Planted (Maybe Date)
    | PlantedValue (Maybe String)
    | Now


type alias Model =
    { newCrop : NewCrop
    }


model : ( Model, Cmd Msg )
model =
    ( Model (NewCrop "" "" Nothing Nothing), now )


updateCrop : (NewCrop -> NewCrop) -> Model -> Model
updateCrop update model =
    { model | newCrop = update model.newCrop }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        Name name ->
            updateCrop (\newCrop -> { newCrop | name = name }) model ! []

        Variety variety ->
            updateCrop (\newCrop -> { newCrop | variety = variety }) model ! []

        PlantedValue plantedValue ->
            updateCrop
                (\newCrop ->
                    { newCrop
                        | plantedValue = plantedValue
                        , planted = plantedValue `Maybe.andThen` parseDate
                    }
                )
                model
                ! []

        Planted planted ->
            updateCrop
                (\newCrop ->
                    { newCrop
                        | plantedValue = Maybe.map formatDate planted
                        , planted = planted
                    }
                )
                model
                ! []

        Now ->
            ( model, now )


parseDate : String -> Maybe Date
parseDate =
    Date.fromString >> Result.toMaybe


sendMessage : a -> Cmd a
sendMessage msg =
    msg |> Task.succeed |> Task.perform (always msg) identity


now : Cmd Msg
now =
    Task.perform (always (Planted Nothing)) (Just >> Planted) Date.now


plantedDate : Model -> String
plantedDate model =
    Maybe.oneOf [ model.newCrop.plantedValue, (Maybe.map formatDate model.newCrop.planted) ]
        |> Maybe.withDefault ""


addCropForm : Model -> Html Msg
addCropForm model =
    Html.form [ class "pure-form pure-form-stacked pure-u-11-12" ]
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
            , input
                [ onDoubleClick Now
                , title "Double click to use today's date"
                , type' "text"
                , value (plantedDate model)
                , onInput (Just >> PlantedValue)
                ]
                []
            ]
        , button [ type' "submit", class "pure-button pure-button-primary" ] [ text "Create" ]
        ]


debug =
    True


listHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Variety" ]
            , th [] [ text "Planted" ]
            ]
        ]


listRow name variety planted =
    tr []
        [ td [] [ text name ]
        , td [] [ text variety ]
        , td [] [ text planted ]
        ]


view : Model -> Html Msg
view model =
    div [ id "main" ]
        -- content
        [ div [ class "pure-g" ]
            [ div [ class "pure-u-1-12" ] []
            , div [ class "pure-u-1-3" ]
                [ table [ class "pure-table pure-table-horizontal" ]
                    [ listHeader
                    , tbody []
                        [ listRow "Lettuce" "Grand Rapids" "2016-10-05"
                        , listRow "Lettuce" "Grand Rapids" "2016-10-05"
                        , listRow "Lettuce" "Grand Rapids" "2016-10-05"
                        , listRow "Lettuce" "Grand Rapids" "2016-10-05"
                        , listRow "Lettuce" "Grand Rapids" "2016-10-05"
                        ]
                    ]
                ]
            , div [ class "pure-u-1-12" ] []
            , div [ class "pure-u-1-3 add-panel" ]
                [ div [ class "pure-u-1-12" ] []
                , div [ class "pure-u-11-12" ]
                    [ addCropForm model
                    ]
                ]
            ]
          -- debug model info
        , div []
            [ text
                (if debug then
                    ""
                 else
                    toString model
                )
            ]
        ]
