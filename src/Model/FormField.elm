module Model.FormField exposing (..)

import Enum exposing (Enum)
import Json.Decode as D
import Json.Encode as E


type FieldRef
    = FFirstname
    | FLastname
    | FBirthdate
    | FBirthplace
    | FAddress
    | FTown
    | FZipcode
    | FPlace
    | FDate
    | FOutingTime


type alias Field =
    { ref : FieldRef
    , id : String
    , placeholder : String
    , value : String
    , valueEdited : Maybe String
    , multiline : Maybe Int
    }


fieldRefEnum : Enum FieldRef
fieldRefEnum =
    Enum.fromIterator
        (\ref ->
            case ref of
                FFirstname ->
                    ( "lastname", FLastname )

                FLastname ->
                    ( "birthdate", FBirthdate )

                FBirthdate ->
                    ( "birthplace", FBirthplace )

                FBirthplace ->
                    ( "address", FAddress )

                FAddress ->
                    ( "town", FTown )

                FTown ->
                    ( "zipcode", FZipcode )

                FZipcode ->
                    ( "place", FPlace )

                FPlace ->
                    ( "date", FDate )

                FDate ->
                    ( "outingTime", FOutingTime )

                FOutingTime ->
                    ( "firstname", FFirstname )
        )
        FFirstname


emptyField : FieldRef -> Field
emptyField ref =
    let
        placeholder =
            case ref of
                FFirstname ->
                    "Prénom"

                FLastname ->
                    "Nom"

                FBirthdate ->
                    "JJ/MM/AAAA"

                FBirthplace ->
                    "Lieux de naissance"

                FAddress ->
                    "Adresse"

                FTown ->
                    "Ville"

                FZipcode ->
                    "Code postal"

                FPlace ->
                    "Lieux"

                FDate ->
                    "JJ/MM/AAAA"

                FOutingTime ->
                    "00h00"

        multiline =
            if ref == FAddress then
                Just 3

            else
                Nothing
    in
    { ref = ref
    , id = "form_field_" ++ fieldRefEnum.toString ref
    , placeholder = placeholder
    , value = ""
    , valueEdited = Nothing
    , multiline = multiline
    }


setValue : v -> { a | value : v } -> { a | value : v }
setValue value field =
    { field | value = value }


encodeField : Field -> E.Value
encodeField field =
    E.object
        [ ( "ref", fieldRefEnum.encode field.ref )
        , ( "value", E.string field.value )
        ]


fieldDecoder : D.Decoder Field
fieldDecoder =
    let
        newField ref value =
            emptyField ref
                |> setValue value
    in
    D.map2 newField
        (D.field "ref" fieldRefEnum.decoder)
        (D.field "value" D.string)
