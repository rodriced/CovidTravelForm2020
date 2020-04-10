port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Config exposing (..)
import Model.FormField exposing (..)
import File
import File.Select as Select
import Helper.Basic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Model.Model exposing (..)
import Motivations exposing (..)
import Msg exposing (..)
import Process
import Task
import Time
import Helper.Time exposing (..)
import View.View exposing (..)



---- PORTS ----


port storeData : E.Value -> Cmd msg


port disableRootScrolling : Bool -> Cmd msg


port print : String -> Cmd msg



---- INIT ----


init : E.Value -> ( Model, Cmd Msg )
init data =
    let
        dataResult =
            D.decodeValue (D.maybe modelDecoder) data

        model =
            case dataResult of
                Err e ->
                    { emptyModel | error = [ e ] }

                Ok maybeModel ->
                    maybeModel
                        |> Maybe.withDefault emptyModel

        fixedModel =
            if formIsValidated model && formIsImcomplete model then
                setValidated Nothing model

            else
                model

        cmd =
            -- if model.locked then
            --     Cmd.none
            -- else
            Task.perform TimeInfoLoaded <|
                Task.map2 TimeInfo Time.now Time.here
    in
    ( fixedModel, cmd )



---- UPDATE HELPERS -----


andStoreDataCmd : Model -> ( Model, Cmd Msg )
andStoreDataCmd model =
    ( model, storeData (encodeModel model) )


andNoCmd : Model -> ( Model, Cmd Msg )
andNoCmd model =
    ( model, Cmd.none )


andCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
andCmd cmd model =
    ( model, cmd )


updateIfValidatedIs : Bool -> Model -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateIfValidatedIs isValidated model thenDo =
    if formIsValidated model == isValidated then
        thenDo

    else
        ( model, Cmd.none )



---- UPDATE -----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> andNoCmd

        FieldEditionStart ref ->
            updateIfValidatedIs False model <|
                let
                    field =
                        getField ref model
                in
                ( model |> setField { field | valueEdited = Just field.value }
                , Task.attempt (\_ -> NoOp) (Dom.focus field.id)
                )

        FieldEditionStop maybeValid ref ->
            updateIfValidatedIs False model <|
                let
                    field =
                        getField ref model
                in
                case maybeValid of
                    Nothing ->
                        model
                            |> setField { field | valueEdited = Nothing }
                            |> andNoCmd

                    Just _ ->
                        let
                            toValue raw =
                                if ref == FDate && String.isEmpty raw then
                                    model.now
                                        |> Maybe.map formatDate
                                        |> Maybe.withDefault raw

                                else if ref == FOutingTime && String.isEmpty raw then
                                    model.now
                                        |> Maybe.map formatTime
                                        |> Maybe.withDefault raw

                                else
                                    raw
                        in
                        case field.valueEdited of
                            Just rawInput ->
                                model
                                    |> setField { field | value = toValue rawInput, valueEdited = Nothing }
                                    |> andStoreDataCmd

                            Nothing ->
                                model
                                    |> andNoCmd

        FieldInput ref rawInput ->
            updateIfValidatedIs False model <|
                let
                    field =
                        getField ref model
                in
                model
                    |> setField { field | valueEdited = Just rawInput }
                    |> andNoCmd

        MotivationCheckboxToggle clickedId ->
            updateIfValidatedIs False model <|
                let
                    newSelectedMotivations =
                        model.motivations
                            |> List.indexedMap
                                (\i isSelected ->
                                    if i == clickedId then
                                        not isSelected

                                    else
                                        isSelected
                                )
                in
                { model | motivations = newSelectedMotivations }
                    |> andStoreDataCmd

        SignatureRequested ->
            updateIfValidatedIs False model <|
                let
                    mimes =
                        [ "bmp", "gif", "jpeg", "png", "svg+xml", "tiff" ]
                            |> List.map (\t -> "image/" ++ t)
                in
                ( model
                , Select.file mimes SignatureSelected
                )

        SignatureSelected file ->
            updateIfValidatedIs False model <|
                ( model
                , Task.perform SignatureLoaded (File.toUrl file)
                )

        SignatureLoaded imageUrl ->
            updateIfValidatedIs False model <|
                (model
                    |> setSignature imageUrl
                    |> andStoreDataCmd
                )

        SignatureRemoveRequested ->
            updateIfValidatedIs False model <|
                (model
                    |> setSignature ""
                    |> andStoreDataCmd
                )

        ValidatedTimeLoaded ti ->
            { model | validated = ti }
                |> andStoreDataCmd

        TimeInfoLoaded now ->
            if formIsValidated model then
                ( { model | now = Just now, zone = now.zone }, Cmd.none )

            else
                { model | now = Just now }
                    |> updateField FDate (setValue (formatDate now))
                    |> andStoreDataCmd

        HelpDisplayStatusToggle ->
            let
                helpDisplayed =
                    not model.helpDisplayed
            in
            { model | helpDisplayed = helpDisplayed }
                |> andCmd (disableRootScrolling helpDisplayed)

        StateButtonToggle ->
            let
                validated =
                    if formIsImcomplete model then
                        False

                    else
                        not (formIsValidated model)

                cmd =
                    -- if model.validated then
                    --     Cmd.none
                    -- else
                    Task.perform ValidatedTimeLoaded <|
                        if validated then
                            Time.now
                                |> Task.map Just

                        else
                            Task.succeed Nothing
            in
            ( model, cmd )

        -- PrintRequested ->
        --    updateIfValidatedIs True model <|
        --     ( { model | printingStarting = True }
        --     , Process.sleep 2000
        --         |> Task.perform (always PrintStart)
        --       -- , Task.succeed ()
        --       --     |> Task.perform (always PrintStart)
        --     )
        -- PrintStart ->
        --    updateIfValidatedIs True model <|
        --     ( model
        --     , Cmd.batch
        --         [ print printId
        --         , Process.sleep 2000
        --             |> Task.perform (always PrintingInProgress)
        --         -- , Task.succeed ()
        --         --     |> Task.perform (always PrintingInProgress)
        --         ]
        --     )
        PrintRequested ->
            updateIfValidatedIs True model <|
                ( { model | printingStarting = True }
                , Cmd.batch
                    [ print printId
                    , Process.sleep 2000
                        |> Task.perform (always PrintingInProgress)
                    ]
                )

        PrintingInProgress ->
            updateIfValidatedIs True model <|
                ( { model | printingStarting = False }, Cmd.none )

        ClearForm ->
            updateIfValidatedIs False model <|
                let
                    clearFields =
                        getFields >> List.map (setValue "")

                    clearMotivations =
                        .motivations >> List.map (always False)
                in
                model
                    |> setFields (clearFields model)
                    |> setMotivations (clearMotivations model)
                    |> setSignature ""
                    |> andStoreDataCmd



---- MAIN ----


main : Program D.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none

        -- , subscriptions = subscriptions
        }
