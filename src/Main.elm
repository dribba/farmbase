module Main exposing (..)

import Html exposing (Html, form, button, div, text, label, input, legend, table, th, tr, td, thead, tbody)
import Html.Events exposing (onInput, onClick, onDoubleClick, onSubmit)
import Html.Attributes exposing (..)
import Utils.MaybeUtils exposing (or)
import Utils.AsyncData exposing (..)
import Date exposing (Date, Month)
import Task exposing (Task)
import Platform.Cmd exposing (Cmd, (!))
import Http exposing (Error, Request)
import Json.Encode exposing (Value)
import Json.Encode as Encode
import Json.Decode exposing (Decoder)
import Json.Decode as Decode
import Routing
import Navigation exposing (Location)
import Routing exposing (parseLocation)
import Models.Crop exposing (..)
import Models.Main exposing (..)
import MainMessages exposing (..)
import MainView exposing (mainView)
import Pages.MyFarm.Update exposing (updateMyFarm)


subscriptions : Model -> Sub MainMessage
subscriptions model =
    Sub.none


main : Program Never Model MainMessage
main =
    Navigation.program OnLocationChange
        { init = model
        , view = mainView
        , update = update
        , subscriptions = subscriptions
        }


model : Location -> ( Model, Cmd MainMessage )
model location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        Model emptyPageData Empty currentRoute ! []



--[ sendMessage RefreshCrops ]
-- ! [ now, sendMessage RefreshCrops ]
-- updateCrop : (CropForm -> CropForm) -> Model -> Model
-- updateCrop update model =
--     { model | cropForm = update model.cropForm }


pageMessage : PageMessage -> Model -> ( Model, Cmd MainMessage )
pageMessage message model =
    case message of
        MyFarmMessage msg ->
            updateMyFarm msg model


update : MainMessage -> Model -> ( Model, Cmd MainMessage )
update msg model =
    case (Debug.log "msg" msg) of
        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        -- Name name ->
        --     updateCrop (\cropForm -> { cropForm | name = name }) model ! []
        --
        -- Variety variety ->
        --     updateCrop (\cropForm -> { cropForm | variety = variety }) model ! []
        --
        -- PlantedValue plantedValue ->
        --     updateCrop
        --         (\cropForm ->
        --             { cropForm
        --                 | plantedValue = plantedValue
        --                 , planted = plantedValue |> Maybe.andThen parseDate
        --             }
        --         )
        --         model
        --         ! []
        --
        -- Planted planted ->
        --     updateCrop
        --         (\cropForm ->
        --             { cropForm
        --                 | plantedValue = Maybe.map formatDate planted
        --                 , planted = planted
        --             }
        --         )
        --         model
        --         ! []
        -- SaveCropForm ->
        --     model
        --         ! [ (if model.cropForm.id == Nothing then
        --                 createCrop model.cropForm
        --              else
        --                 saveCrop model.cropForm
        --             )
        --           ]
        --
        -- CropSaved crop ->
        --     model ! [ sendMessage RefreshCrops, sendMessage ResetCropForm ]
        --
        -- CropSaveError error ->
        --     model ! []
        ReceiveCrops crops ->
            { model | crops = Ready crops } ! []

        -- CropsError error ->
        --     { model | crops = Failed error } ! []
        --
        -- RemoveCrop crop ->
        --     model ! [ deleteCrop crop ]
        --
        -- CropRemoved ->
        --     model ! [ sendMessage RefreshCrops ]
        --
        -- CropRemoveError ->
        --     model ! []
        --
        -- EditCrop crop ->
        --     { model | cropForm = CropForm crop.name crop.variety crop.planted Nothing (Just crop.id) } ! []
        --
        -- ResetCropForm ->
        --     { model | cropForm = emptyCropForm } ! [ sendMessage Now ]
        RefreshCrops ->
            { model | crops = Pending } ! []

        --[ getCrops ]
        PageMessage msg ->
            pageMessage msg model

        Now ->
            model ! []



-- ( model, now )


parseDate : String -> Maybe Date
parseDate =
    Date.fromString >> Result.toMaybe


sendMessage : a -> Cmd a
sendMessage msg =
    msg |> Task.succeed |> Task.perform (always msg)



-- now : Cmd MainMessage
-- now =
--     Task.perform (always (Planted Nothing)) (Task.map (Just >> Planted) Date.now)


wrapResult : (Error -> b) -> (a -> b) -> Result Error a -> b
wrapResult error success result =
    case result of
        Ok value ->
            success value

        Err err ->
            error err



-- getCrops : Cmd MainMessage
-- getCrops =
--     let
--         url =
--             "http://localhost:9000/api/v1/crops"
--     in
--         Http.send (wrapResult CropsError ReceiveCrops) (Http.get url (Decode.list decodeCrop))
-- createCrop : CropForm -> Cmd MainMessage
-- createCrop cropForm =
--     let
--         url =
--             "http://localhost:9000/api/v1/crops"
--     in
--         Http.send (wrapResult CropSaveError CropSaved) (postJson decodeCrop url (cropForm |> encodeNewCrop))
-- saveCrop : CropForm -> Cmd MainMessage
-- saveCrop cropForm =
--     let
--         url =
--             "http://localhost:9000/api/v1/crops/" ++ (or cropForm.id "")
--     in
--         Http.send (wrapResult CropSaveError CropSaved) (putJson decodeCrop url (cropForm |> encodeNewCrop))
-- deleteCrop : Crop -> Cmd MainMessage
-- deleteCrop crop =
--     let
--         url =
--             "http://localhost:9000/api/v1/crops/" ++ crop.id
--     in
--         Http.send (wrapResult (always CropRemoveError) (always CropRemoved)) (httpDelete (Decode.succeed "") url)
-- plantedDate : CropForm -> String
-- plantedDate cropForm =
--     case ( cropForm.plantedValue, (Maybe.map formatDate cropForm.planted) ) of
--         ( Just date, _ ) ->
--             date
--
--         ( _, Just date ) ->
--             date
--
--         ( _, _ ) ->
--             ""
-- cropFormView : CropForm -> Html MainMessage
-- cropFormView cropForm =
--     let
--         isEdit =
--             cropForm.id /= Nothing
--
--         createButton =
--             [ button [ type_ "submit", class "pure-button pure-button-primary pure-u-1-4" ] [ text "Create" ]
--             , text " "
--             , button [ type_ "button", class "pure-button pure-button-primary pure-u-1-4", onClick ResetCropForm ] [ text "Reset" ]
--             ]
--
--         saveAndCancelButtons =
--             [ button [ type_ "submit", class "pure-button pure-button-primary pure-u-1-4" ] [ text "Save" ]
--             , text " "
--             , button [ type_ "button", class "pure-button pure-button-primary pure-u-1-4", onClick ResetCropForm ] [ text "Cancel" ]
--             ]
--     in
--         Html.form [ class "pure-form pure-form-stacked pure-u-11-12", onSubmit SaveCropForm ]
--             [ legend [] [ text "Add Crop" ]
--             , div []
--                 [ label [] [ text "Name" ]
--                 , input [ type_ "text", class "pure-u-1-2", value cropForm.name, placeholder "Lettuce", onInput Name ] []
--                 ]
--             , div []
--                 [ label [] [ text "Variety" ]
--                 , input [ type_ "text", class "pure-u-1-2", value cropForm.variety, placeholder "Grand Rapids", onInput Variety ] []
--                 ]
--             , div []
--                 [ label [] [ text "Date" ]
--                 , input
--                     [ onDoubleClick Now
--                     , title "Double click to use today's date"
--                     , type_ "text"
--                     , class "pure-u-1-2"
--                     , value (plantedDate cropForm)
--                     , onInput (Just >> PlantedValue)
--                     ]
--                     []
--                 ]
--             , div []
--                 (if isEdit then
--                     saveAndCancelButtons
--                  else
--                     createButton
--                 )
--             ]


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



-- listRow : Crop -> Html MainMessage
-- listRow ({ name, variety, planted } as crop) =
--     tr []
--         [ td [] [ text name ]
--         , td [] [ text variety ]
--         , td [] [ text (or (Maybe.map formatDate planted) "") ]
--         , td []
--             [ button [ title "Edit", onClick (EditCrop crop) ] [ text "🖊" ]
--             , button [ title "Delete", onClick (RemoveCrop crop) ] [ text "🔥" ]
--             ]
--         ]
--
--
-- cropTable : List Crop -> Html MainMessage
-- cropTable crops =
--     div []
--         [ table [ class "pure-table pure-table-horizontal" ]
--             [ listHeader
--             , tbody [] (List.map listRow crops)
--             ]
--         ]
