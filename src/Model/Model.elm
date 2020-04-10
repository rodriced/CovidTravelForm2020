module Model.Model exposing (..)

import Helper.Basic exposing (..)
import Helper.Time exposing (..)
import Json.Decode as D
import Json.Encode as E
import Model.FormField exposing (..)
import Motivations exposing (..)
import Time


type alias Model =
    { firstname : Field
    , lastname : Field
    , birthdate : Field
    , birthplace : Field
    , address : Field
    , town : Field
    , zipcode : Field
    , place : Field
    , date : Field
    , outingTime : Field
    , motivations : List Bool
    , signature : String
    , zone : Time.Zone
    , now : Maybe TimeInfo
    , validated : Maybe Time.Posix
    , helpDisplayed : Bool
    , printingStarting : Bool
    , error : List D.Error
    }


emptyModel : Model
emptyModel =
    { firstname = emptyField FFirstname
    , lastname = emptyField FLastname
    , birthdate = emptyField FBirthdate
    , birthplace = emptyField FBirthplace
    , address = emptyField FAddress
    , town = emptyField FTown
    , zipcode = emptyField FZipcode
    , place = emptyField FPlace
    , date = emptyField FDate
    , outingTime = emptyField FOutingTime
    , motivations = emptyMotivations
    , signature = ""
    , zone = Time.utc
    , now = Nothing
    , validated = Nothing
    , helpDisplayed = False
    , printingStarting = False
    , error = []
    }


setMotivations : v -> { a | motivations : v } -> { a | motivations : v }
setMotivations v model =
    { model | motivations = v }


setSignature : v -> { a | signature : v } -> { a | signature : v }
setSignature v model =
    { model | signature = v }


setValidated : v -> { a | validated : v } -> { a | validated : v }
setValidated v model =
    { model | validated = v }


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "fields", E.list encodeField (getFields model) )
        , ( "motivations", E.list E.bool model.motivations )
        , ( "signature", E.string model.signature )
        , ( "validated"
          , model.validated
                |> Maybe.map (Time.posixToMillis >> E.int)
                |> Maybe.withDefault E.null
          )
        ]


modelDecoder : D.Decoder Model
modelDecoder =
    let
        newModel fields motivations signature validated =
            emptyModel
                |> setFields fields
                |> setMotivations
                    (ifTrue (List.length motivations == List.length motivationsDescription)
                        motivations
                        emptyMotivations
                    )
                |> setSignature signature
                |> setValidated validated
    in
    D.map4 newModel
        (D.field "fields" <| D.list fieldDecoder)
        (D.field "motivations" <| D.list D.bool)
        (D.field "signature" D.string)
        (D.field "validated" <| D.maybe <| D.map Time.millisToPosix D.int)


getField : FieldRef -> Model -> Field
getField ref model =
    case ref of
        FFirstname ->
            model.firstname

        FLastname ->
            model.lastname

        FBirthdate ->
            model.birthdate

        FBirthplace ->
            model.birthplace

        FAddress ->
            model.address

        FTown ->
            model.town

        FZipcode ->
            model.zipcode

        FPlace ->
            model.place

        FDate ->
            model.date

        FOutingTime ->
            model.outingTime


setField : Field -> Model -> Model
setField field model =
    case field.ref of
        FFirstname ->
            { model | firstname = field }

        FLastname ->
            { model | lastname = field }

        FBirthdate ->
            { model | birthdate = field }

        FBirthplace ->
            { model | birthplace = field }

        FAddress ->
            { model | address = field }

        FTown ->
            { model | town = field }

        FZipcode ->
            { model | zipcode = field }

        FPlace ->
            { model | place = field }

        FDate ->
            { model | date = field }

        FOutingTime ->
            { model | outingTime = field }


updateField : FieldRef -> (Field -> Field) -> Model -> Model
updateField ref updateFieldFn model =
    let
        field =
            getField ref model
    in
    model
        |> setField (updateFieldFn field)


getFields : Model -> List Field
getFields model =
    fieldRefEnum.list
        |> List.map
            (\( _, ref ) ->
                getField ref model
            )


setFields : List Field -> Model -> Model
setFields fields model =
    List.foldl setField model fields


formIsValidated : Model -> Bool
formIsValidated model =
    model.validated /= Nothing


formIsImcomplete : Model -> Bool
formIsImcomplete model =
    someFieldsAreImcomplete model
        || noneMotivationIsChosen model



-- || String.isEmpty model.signature


someFieldsAreImcomplete : Model -> Bool
someFieldsAreImcomplete model =
    model
        |> getFields
        |> List.any (.value >> String.isEmpty)


noneMotivationIsChosen : Model -> Bool
noneMotivationIsChosen model =
    List.all
        ((==) False)
        model.motivations
