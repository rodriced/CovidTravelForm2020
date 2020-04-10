module View.QrCodeView exposing (..)

import Helper.Basic exposing (..)
import Helper.Time exposing (..)
import Html exposing (Html, figure, text)
import Html.Attributes exposing (class)
import List.Extra as List
import Model.Model exposing (..)
import QRCode


qrCodeView : Model -> Html msg
qrCodeView model =
    let
        reasons =
            [ "travail", "courses", "sante", "famille", "sport", "judiciaire", "missions" ]

        selectedReasons =
            List.zip reasons model.motivations
                |> List.filterMap (\( reason, selected ) -> ifTrue selected (Just reason) Nothing)
                -- |> List.intersperse "-"
                |> String.join "-"

        data =
            [ "Cree le: "
                ++ (model.validated
                        |> Maybe.map (posixToDate model.zone)
                        |> Maybe.withDefault ""
                   )
                ++ " a "
                ++ (model.validated
                        |> Maybe.map (posixToTime model.zone)
                        |> Maybe.withDefault ""
                   )
            , "Nom: " ++ model.lastname.value
            , "Prenom: " ++ model.firstname.value
            , "Naissance: " ++ model.birthdate.value ++ " a " ++ model.birthplace.value
            , "Adresse: " ++ model.address.value ++ " " ++ model.zipcode.value ++ " " ++ model.town.value
            , "Sortie: " ++ model.date.value ++ " a " ++ String.replace ":" "h" model.outingTime.value
            , "Motifs: " ++ selectedReasons
            ]
                |> String.join "; "
    in
    QRCode.encode data
        |> Result.map QRCode.toSvg
        |> Result.withDefault (text "QRcode Error !")
        |> List.singleton
        |> figure [ class "image is-square" ]
