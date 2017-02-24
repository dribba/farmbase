module Pages.MyFarm.Update exposing (..)

import MainMessages exposing (..)
import Pages.MyFarm.Types exposing (..)
import Models.Crop exposing (..)
import Models.Main exposing (Model)
import Utils.Http exposing (..)
import Apis.CropApi exposing (..)
import Http
import Navigation
import Pages.MyFarm.Routing as MyFarmRoute


updateMyFarm : MyFarmMessage -> Model -> ( Model, Cmd MainMessage )
updateMyFarm msg model =
    case msg of
        AddCropFormMessage msg ->
            let
                ( newModel, cmd ) =
                    addCropFormUpdate msg model.pageData.myFarm.addCropForm
            in
                updateAddCropForm model newModel ! [ cmd ]


updateAddCropForm : Model -> CropForm -> Model
updateAddCropForm model addCropForm =
    let
        pageData =
            model.pageData

        myFarm =
            model.pageData.myFarm
    in
        { model | pageData = { pageData | myFarm = { myFarm | addCropForm = addCropForm } } }


addCropFormUpdate : AddCropFormMessage -> CropForm -> ( CropForm, Cmd MainMessage )
addCropFormUpdate msg addCropForm =
    case msg of
        CropTypeUpd type_ ->
            { addCropForm | type_ = type_ } ! []

        CropVarietyUpd variety ->
            { addCropForm | variety = variety } ! []

        CropQtyUpd qty ->
            { addCropForm | quantity = qty } ! []

        CropMediaUpd media ->
            { addCropForm | media = media } ! []

        CropFeedingTypeUpd feeding ->
            { addCropForm | feeding = feeding } ! []

        AddCropResponse reponse ->
            -- { addCropForm | feeding = feeding }
            case reponse of
                Ok _ ->
                    addCropForm ! [ Navigation.newUrl MyFarmRoute.index ]

                Err e ->
                    (Debug.log ("Found error :(" ++ (toString e)) addCropForm) ! []

        Submit ->
            addCropForm ! [ Cmd.map (AddCropFormMessage >> MyFarmMessage >> PageMessage) (createCrop addCropForm) ]



-- saveCrop : CropForm -> Cmd MainMessage
-- saveCrop cropForm =
--     let
--         url =
--             "http://localhost:9000/api/v1/crops/" ++ (or cropForm.id "")
--     in
--         Http.send (wrapResult CropSaveError CropSaved) (putJson decodeCrop url (cropForm |> encodeNewCrop))


createCrop : CropForm -> Cmd AddCropFormMessage
createCrop cropForm =
    Http.send AddCropResponse (postJson decodeCrop addCropEndpoint (cropForm |> encodeCropForm))
