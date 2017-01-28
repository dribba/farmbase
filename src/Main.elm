module Main exposing (..)

import Html exposing (Html, form, button, div, text, label, input, legend, table, th, tr, td, thead, tbody)
import Html.Events exposing (onInput, onClick, onDoubleClick, onSubmit)
import Html.Attributes exposing (..)
import Utils.MaybeUtils exposing (or)
import Utils.DateUtils exposing (formatDate, decodeDate)
import Date exposing (Date, Month)
import Task exposing (Task)
import Platform.Cmd exposing (Cmd, (!))
import Http exposing (Error, Request)
import Json.Encode exposing (Value)
import Json.Encode as Encode
import Json.Decode exposing (Decoder)
import Json.Decode as Decode
import Routing
import MainView
import Navigation exposing (Location)
import Routing exposing (parseLocation)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = model
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type AsyncData b a
    = Empty
    | Loading
    | Success a
    | Error b


type alias Crop =
    { id : String
    , name : String
    , variety : String
    , planted : Maybe Date
    }


type alias CropForm =
    { name : String
    , variety : String
    , planted : Maybe Date
    , plantedValue : Maybe String
    , id : Maybe String
    }


emptyCropForm : CropForm
emptyCropForm =
    CropForm "" "" Nothing Nothing Nothing


type Msg
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


type alias Model =
    { cropForm : CropForm
    , crops : AsyncData Error (List Crop)
    , route : Routing.Route
    }


model : Location -> ( Model, Cmd Msg )
model location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        Model emptyCropForm Empty currentRoute ! [ now, sendMessage RefreshCrops ]


updateCrop : (CropForm -> CropForm) -> Model -> Model
updateCrop update model =
    { model | cropForm = update model.cropForm }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        Name name ->
            updateCrop (\cropForm -> { cropForm | name = name }) model ! []

        Variety variety ->
            updateCrop (\cropForm -> { cropForm | variety = variety }) model ! []

        PlantedValue plantedValue ->
            updateCrop
                (\cropForm ->
                    { cropForm
                        | plantedValue = plantedValue
                        , planted = plantedValue |> Maybe.andThen parseDate
                    }
                )
                model
                ! []

        Planted planted ->
            updateCrop
                (\cropForm ->
                    { cropForm
                        | plantedValue = Maybe.map formatDate planted
                        , planted = planted
                    }
                )
                model
                ! []

        SaveCropForm ->
            model
                ! [ (if model.cropForm.id == Nothing then
                        createCrop model.cropForm
                     else
                        saveCrop model.cropForm
                    )
                  ]

        CropSaved crop ->
            model ! [ sendMessage RefreshCrops, sendMessage ResetCropForm ]

        CropSaveError error ->
            model ! []

        ReceiveCrops crops ->
            { model | crops = Success crops } ! []

        CropsError error ->
            { model | crops = Error error } ! []

        RemoveCrop crop ->
            model ! [ deleteCrop crop ]

        CropRemoved ->
            model ! [ sendMessage RefreshCrops ]

        CropRemoveError ->
            model ! []

        EditCrop crop ->
            { model | cropForm = CropForm crop.name crop.variety crop.planted Nothing (Just crop.id) } ! []

        ResetCropForm ->
            { model | cropForm = emptyCropForm } ! [ sendMessage Now ]

        RefreshCrops ->
            { model | crops = Loading } ! [ getCrops ]

        Now ->
            ( model, now )


parseDate : String -> Maybe Date
parseDate =
    Date.fromString >> Result.toMaybe


sendMessage : a -> Cmd a
sendMessage msg =
    msg |> Task.succeed |> Task.perform (always msg)


now : Cmd Msg
now =
    Task.perform (always (Planted Nothing)) (Task.map (Just >> Planted) Date.now)


decodeCrop : Decoder Crop
decodeCrop =
    Decode.map4 Crop
        (Decode.field "_id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "variety" Decode.string)
        (Decode.field "planted" decodeDate)


encodeNewCrop : CropForm -> Value
encodeNewCrop cropForm =
    Encode.object
        [ ( "name", Encode.string cropForm.name )
        , ( "variety", Encode.string cropForm.variety )
        , ( "planted", Encode.string (or (Maybe.map formatDate cropForm.planted) "") )
        ]


wrapResult : (Error -> b) -> (a -> b) -> Result Error a -> b
wrapResult error success result =
    case result of
        Ok value ->
            success value

        Err err ->
            error err


getCrops : Cmd Msg
getCrops =
    let
        url =
            "http://localhost:9000/api/v1/crops"
    in
        Http.send (wrapResult CropsError ReceiveCrops) (Http.get url (Decode.list decodeCrop))


postJson : Decoder value -> String -> Value -> Request value
postJson decoder url body =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


putJson : Decoder value -> String -> Value -> Request value
putJson decoder url body =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


httpDelete : Decoder value -> String -> Request value
httpDelete decoder url =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


createCrop : CropForm -> Cmd Msg
createCrop cropForm =
    let
        url =
            "http://localhost:9000/api/v1/crops"
    in
        Http.send (wrapResult CropSaveError CropSaved) (postJson decodeCrop url (cropForm |> encodeNewCrop))


saveCrop : CropForm -> Cmd Msg
saveCrop cropForm =
    let
        url =
            "http://localhost:9000/api/v1/crops/" ++ (or cropForm.id "")
    in
        Http.send (wrapResult CropSaveError CropSaved) (putJson decodeCrop url (cropForm |> encodeNewCrop))


deleteCrop : Crop -> Cmd Msg
deleteCrop crop =
    let
        url =
            "http://localhost:9000/api/v1/crops/" ++ crop.id
    in
        Http.send (wrapResult (always CropRemoveError) (always CropRemoved)) (httpDelete (Decode.succeed "") url)


plantedDate : CropForm -> String
plantedDate cropForm =
    case ( cropForm.plantedValue, (Maybe.map formatDate cropForm.planted) ) of
        ( Just date, _ ) ->
            date

        ( _, Just date ) ->
            date

        ( _, _ ) ->
            ""


cropFormView : CropForm -> Html Msg
cropFormView cropForm =
    let
        isEdit =
            cropForm.id /= Nothing

        createButton =
            [ button [ type_ "submit", class "pure-button pure-button-primary pure-u-1-4" ] [ text "Create" ]
            , text " "
            , button [ type_ "button", class "pure-button pure-button-primary pure-u-1-4", onClick ResetCropForm ] [ text "Reset" ]
            ]

        saveAndCancelButtons =
            [ button [ type_ "submit", class "pure-button pure-button-primary pure-u-1-4" ] [ text "Save" ]
            , text " "
            , button [ type_ "button", class "pure-button pure-button-primary pure-u-1-4", onClick ResetCropForm ] [ text "Cancel" ]
            ]
    in
        Html.form [ class "pure-form pure-form-stacked pure-u-11-12", onSubmit SaveCropForm ]
            [ legend [] [ text "Add Crop" ]
            , div []
                [ label [] [ text "Name" ]
                , input [ type_ "text", class "pure-u-1-2", value cropForm.name, placeholder "Lettuce", onInput Name ] []
                ]
            , div []
                [ label [] [ text "Variety" ]
                , input [ type_ "text", class "pure-u-1-2", value cropForm.variety, placeholder "Grand Rapids", onInput Variety ] []
                ]
            , div []
                [ label [] [ text "Date" ]
                , input
                    [ onDoubleClick Now
                    , title "Double click to use today's date"
                    , type_ "text"
                    , class "pure-u-1-2"
                    , value (plantedDate cropForm)
                    , onInput (Just >> PlantedValue)
                    ]
                    []
                ]
            , div []
                (if isEdit then
                    saveAndCancelButtons
                 else
                    createButton
                )
            ]


debug =
    True


listHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Variety" ]
            , th [] [ text "Planted" ]
            , th [] [ text "Actions" ]
            ]
        ]


listRow : Crop -> Html Msg
listRow ({ name, variety, planted } as crop) =
    tr []
        [ td [] [ text name ]
        , td [] [ text variety ]
        , td [] [ text (or (Maybe.map formatDate planted) "") ]
        , td []
            [ button [ title "Edit", onClick (EditCrop crop) ] [ text "🖊" ]
            , button [ title "Delete", onClick (RemoveCrop crop) ] [ text "🔥" ]
            ]
        ]


cropTable : List Crop -> Html Msg
cropTable crops =
    div []
        [ table [ class "pure-table pure-table-horizontal" ]
            [ listHeader
            , tbody [] (List.map listRow crops)
            ]
        ]


view : Model -> Html Msg
view model =
    div [ id "main" ]
        -- content
        [ MainView.navBar
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-12" ] []
            , div [ class "pure-u-1-3" ]
                [ (case model.crops of
                    Empty ->
                        text "Loading..."

                    Loading ->
                        text "Loading..."

                    Success crops ->
                        cropTable crops

                    Error e ->
                        text "Oh no! There was an error :("
                  )
                , button [ title "Refresh", onClick RefreshCrops ] [ text "♻️" ]
                ]
            , div [ class "pure-u-1-12" ] []
            , div [ class "pure-u-1-3 add-panel" ]
                [ div [ class "pure-u-1-12" ] []
                , div [ class "pure-u-11-12" ]
                    [ cropFormView model.cropForm
                    ]
                ]
            ]
          -- debug model info
        , div []
            [ text
                (if debug then
                    toString model
                 else
                    ""
                )
            ]
        ]
