module Main exposing (..)

import Html exposing (Html, form, button, div, text, label, input, legend, table, th, tr, td, thead, tbody)
import Html.App as App
import Html.Events exposing (onInput, onDoubleClick)
import Html.Attributes exposing (..)
import Utils.MaybeUtils exposing (or)
import Utils.DateUtils exposing (formatDate)
import Date exposing (Date, Month)
import Task


main =
    App.program { init = model, view = view, update = update, subscriptions = \_ -> Sub.none }


type alias NewCrop =
    { name : String
    , variety : String
    , germinated : Maybe Date
    , germinatedValue : Maybe String
    }


type Msg
    = Name String
    | Variety String
    | Germinated (Maybe String)
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
            ( updateCrop (\newCrop -> { newCrop | name = name }) model, Cmd.none )

        Variety variety ->
            ( updateCrop (\newCrop -> { newCrop | variety = variety }) model, Cmd.none )

        Germinated germinated ->
            let
                newCrop =
                    model.newCrop
            in
                ( { model | newCrop =
                        { newCrop
                            | germinated = germinated `Maybe.andThen` parseDate
                            , germinatedValue = germinated
                        }
                  }
                , Cmd.none
                )

        Now ->
            ( model, now )


parseDate : String -> Maybe Date
parseDate =
  Date.fromString >> Result.toMaybe

now : Cmd Msg
now =
    Task.perform (always (Germinated Nothing)) (formatDate >> Just >> Germinated) Date.now


defaultDate model =
    model.newCrop.germinated
        |> Maybe.map formatDate
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
                , value (model.newCrop.germinatedValue `or` "")
                , onInput (Just >> Germinated)
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
