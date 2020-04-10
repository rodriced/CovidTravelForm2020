module View.View exposing (..)

import Config exposing (..)
import Helper.Basic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode as D
import Keyboard as Kb exposing (Key(..))
import Keyboard.Events as Kb
import Markdown
import Model.FormField exposing (..)
import Model.Model exposing (..)
import Motivations exposing (..)
import Msg exposing (..)
import View.QrCodeView exposing (..)


helpText : Html msg
helpText =
    Markdown.toHtml [ class "content" ] """
### Comment se servir de ce formulaire

#### En résumé
Pour pouvoir remplir le formulaire, le cadenas doit être déverrouillé ;
- remplissez tous les champs (la date de signature doit déjà être à jour),
- cliquez sur '(signature)', prenez une photo de la votre et chargez-là,
- cliquez sur le cadenas pour le verrouiller, l'attestation n'est alors plus modifiable.

Remarques :
- quand le cadenas est déverrouillé et que l'on recharge la page, la date de signature se met automatiquement à jour ;
- rechargez régulièrment la page pour bénéficier d'éventuelles mises à jour et de corrections de bugs.

#### Compléments

Signification des couleurs :
- champs rouge -> modifiable et manquant,
- champs bleu  -> modifiable,
- cadenas rouge -> tout n'est pas rempli dans le formulaire, le cadenas n'est pas verrouillable,
- cadenas jaune -> tout est bien saisi, le cadenas est verrouillable
- tout noir -> tout est verrouillé, sauf cadenas et informations qui sont toujours cliquables.

Les informations sont automatiquements enregistrées dans votre navigateur.
Si vous le fermez et que vous revenez plus tard, ou que vous rechargez la page, elles sont toujours là.

Une fois saisis, les textes des champs se mettent en gras et bleus.
Vous pouvez toujours les modifier en cliquant dessus. Idem pour la signature.

Quand vous verrouillez le cadenas en haut à gauche en cliquant dessus, les champs ne sont plus saisissables et tous le texte se met en noir.

Si vous voulez à nouveau le modifier, il faut deverrouiller le cadenas en cliquant à nouveau dessus.

Quand vous rechargez la page et que le cadenans est ouvert, la date pour la signature se met automatiquement à jour.
On peut toujours la modifier manuellement en cas de besoin.

### Vie privée
Cette application fonctionne uniquement dans votre navigateur, de manière autonome, et ne communique avec aucun serveur.
Les données que vous saisissez sont stockées dans la page même, de manière locale et privée.
Il n'y a pas d'usage de cookies.

### Informations techniques
Cette application a été réalisé en Elm (https://elm-lang.org/) un langage fonctionnel pour le développement d'application Web.
Les données du formulaire sont stockées dans le localStorage de la page, sous la clé 'covid.form.data'.
"""


helpModal : Model -> Html Msg
helpModal model =
    div
        [ class "modal"
        , classList [ ( "is-active", model.helpDisplayed ) ]
        , Kb.on Kb.Keypress [ ( Kb.Escape, HelpDisplayStatusToggle ) ]
        ]
        [ div [ class "modal-background", onClick HelpDisplayStatusToggle ]
            []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head has-background-success" ]
                [ p [ class "modal-card-title" ]
                    [ text "Informations et aide" ]
                , button [ class "delete", onClick HelpDisplayStatusToggle ]
                    []
                ]
            , section [ class "modal-card-body" ]
                [ helpText ]

            -- , footer [ class "modal-card-foot" ]
            --     [ button [ class "button is-success" ]
            --         [ text "Save changes" ]
            --     , button [ class "button" ]
            --         [ text "Cancel" ]
            --     ]
            ]

        -- , div [ class "modal-content" ]
        --     []
        -- , button [ class "modal-close is-large", onClick HelpDisplayStatusToggle ]
        --     []
        ]


textLines : String -> List (Html msg)
textLines t =
    t |> String.split "\n" |> List.map text |> List.intersperse (br [] [])


horizontalInputField3 : Bool -> String -> Field -> Html Msg
horizontalInputField3 isLocked inputLabel field =
    let
        isEmpty =
            field.valueEdited
                |> Maybe.map String.isEmpty
                |> Maybe.withDefault (String.isEmpty field.value)

        inEdition =
            isEmpty || field.valueEdited /= Nothing

        -- staticField =
        --     strong
        --         [ hidden inEdition
        --         , classList
        --             [ ( "is-hidden", inEdition )
        --             , ( "has-text-link", not isLocked )
        --             ]
        --         , onClick (FieldEditionStart field.ref)
        --         , style "padding-top" "calc(.5em - 1px)"
        --         ]
        --         (textLines field.value)
        inputParams isStatic =
            [ id field.id
            , placeholder field.placeholder
            ]
                ++ (if isStatic then
                        [ class "is-static"
                        , classList [ ( "is-hidden", inEdition ) ]
                        , readonly True
                        , value field.value
                        ]

                    else
                        [ value (field.valueEdited |> Maybe.withDefault "")
                        , onInput (FieldInput field.ref)
                        , onBlur (FieldEditionStop (Just Valid) field.ref)
                        , Kb.on Kb.Keydown [ ( Kb.Escape, FieldEditionStop Nothing field.ref ) ]
                        , classList
                            [ ( "is-danger", isEmpty )
                            , ( "is-hidden", not inEdition )
                            ]
                        ]
                   )

        inputField isStatic =
            case field.multiline of
                Nothing ->
                    input
                        (class "input"
                            :: type_ "text"
                            :: Kb.on Kb.Keypress [ ( Kb.Enter, FieldEditionStop (Just Valid) field.ref ) ]
                            :: inputParams isStatic
                        )
                        []

                Just nbLines ->
                    textarea
                        (class "textarea"
                            :: rows nbLines
                            :: inputParams isStatic
                        )
                        []
    in
    horizontalLayout3 (not inEdition)
        inputLabel
        -- [ inputField, staticField ]
        -- [ inputField (not inEdition) ]
        [ inputField False, inputField True ]


horizontalLayout3 : Bool -> String -> List (Html Msg) -> Html Msg
horizontalLayout3 isStatic inputLabel inputHtml =
    div []
        -- [ div [ class "field is-horizontal is-hidden-mobile" ]
        [ div [ class "field is-horizontal" ]
            -- [ div [ class "field is-horizontal is-flex-mobile" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label has-text-weight-normal" ]
                    [ inputLabel |> toNonBreakableSpace |> text ]
                ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ div
                        (class "control"
                            :: ifTrue isStatic
                                -- [ style "padding-top" ".35em", style "padding-left" ".7em" ]
                                []
                                []
                        )
                        inputHtml
                    ]
                ]
            ]

        -- , div [ class "columns is-mobile is-hidden-tablet" ]
        --     [ div [ class "column is-3" ]
        --         [ text inputLabel
        --         ]
        --     , div [ class "column" ]
        --         inputHtml
        --     ]
        ]


horizontalInputField2 : Bool -> String -> Field -> Html Msg
horizontalInputField2 isLocked inputLabel field =
    let
        isEmpty =
            field.valueEdited
                |> Maybe.map String.isEmpty
                |> Maybe.withDefault (String.isEmpty field.value)

        inEdition =
            isEmpty || field.valueEdited /= Nothing

        inputParams =
            [ id field.id
            , classList [ ( "is-danger", isEmpty ), ( "is-hidden", not inEdition ) ]
            , hidden (not inEdition)
            , placeholder field.placeholder
            , value (field.valueEdited |> Maybe.withDefault "")
            , onInput (FieldInput field.ref)
            , onBlur (FieldEditionStop (Just Valid) field.ref)
            , Kb.on Kb.Keydown [ ( Kb.Escape, FieldEditionStop Nothing field.ref ) ]
            ]
    in
    -- horizontalLayoutCol2 (not inEdition)
    horizontalLayoutField2 (not inEdition)
        inputLabel
        [ strong
            [ hidden inEdition
            , classList
                [ ( "is-hidden", inEdition )
                , ( "has-text-link", not isLocked )
                ]
            , onClick (FieldEditionStart field.ref)
            , style "padding-top" "calc(.5em - 1px)"
            ]
            (textLines field.value)
        , case field.multiline of
            Nothing ->
                input
                    (class "input"
                        :: type_ "text"
                        :: Kb.on Kb.Keypress [ ( Kb.Enter, FieldEditionStop (Just Valid) field.ref ) ]
                        :: inputParams
                    )
                    []

            Just nbLines ->
                textarea
                    (class "textarea"
                        :: rows nbLines
                        :: inputParams
                    )
                    []
        ]


horizontalInputField2b : Bool -> String -> Field -> Html Msg
horizontalInputField2b isLocked inputLabel field =
    let
        isEmpty =
            field.valueEdited
                |> Maybe.map String.isEmpty
                |> Maybe.withDefault (String.isEmpty field.value)

        inEdition =
            isEmpty || field.valueEdited /= Nothing

        inputParams =
            [ id field.id
            , classList [ ( "is-danger", isEmpty ), ( "is-hidden", not inEdition ) ]
            , hidden (not inEdition)
            , placeholder field.placeholder
            , value (field.valueEdited |> Maybe.withDefault "")
            , onInput (FieldInput field.ref)
            , onBlur (FieldEditionStop (Just Valid) field.ref)
            , Kb.on Kb.Keydown [ ( Kb.Escape, FieldEditionStop Nothing field.ref ) ]
            ]
    in
    -- horizontalLayoutCol2 (not inEdition)
    horizontalLayoutField2 (not inEdition)
        inputLabel
        [ strong
            [ hidden inEdition
            , classList
                [ ( "is-hidden", inEdition )
                , ( "has-text-link", not isLocked )
                ]
            , onClick (FieldEditionStart field.ref)
            , style "padding-top" "calc(.5em - 1px)"
            ]
            (textLines field.value)
        , case field.multiline of
            Nothing ->
                input
                    (class "input"
                        :: type_ "text"
                        :: Kb.on Kb.Keypress [ ( Kb.Enter, FieldEditionStop (Just Valid) field.ref ) ]
                        :: inputParams
                    )
                    []

            Just nbLines ->
                textarea
                    (class "textarea"
                        :: rows nbLines
                        :: inputParams
                    )
                    []
        ]


horizontalLayoutField2 : Bool -> String -> List (Html Msg) -> Html Msg
horizontalLayoutField2 isStatic inputLabel inputHtml =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label has-text-weight-normal" ]
                [ inputLabel |> toNonBreakableSpace |> text ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div
                    (class "control"
                        :: ifTrue isStatic
                            [ style "padding-top" ".35em", style "padding-left" ".7em" ]
                            []
                    )
                    inputHtml
                ]
            ]
        ]


horizontalLayoutCol2 : Bool -> String -> List (Html Msg) -> Html Msg
horizontalLayoutCol2 isStatic inputLabel inputHtml =
    div [ class "columns" ]
        [ div [ class "column is-3" ]
            [ label
                [ class "label has-text-weight-normal"

                -- , styleIf (not isStatic) "margin-top" ".50em"
                ]
                [ inputLabel |> toNonBreakableSpace |> text ]
            ]
        , div [ class "column" ]
            [ div [ class "field" ]
                [ div
                    [ class "control"

                    -- , styleIf (not isStatic) "margin-bottom" ".25em"
                    ]
                    inputHtml
                ]
            ]
        ]



-- ]
-- inputFieldHtml : Bool -> Field -> Html Msg
-- inputFieldHtml isLocked field =
--     let
--         isEmpty =
--             field.valueEdited
--                 |> Maybe.map String.isEmpty
--                 |> Maybe.withDefault (String.isEmpty field.value)
--         inEdition =
--             isEmpty || field.valueEdited /= Nothing
--         inputParams =
--             [ id field.id
--             , classList [ ( "is-danger", isEmpty ), ( "is-hidden", not inEdition ) ]
--             , hidden (not inEdition)
--             , placeholder field.placeholder
--             , value (field.valueEdited |> Maybe.withDefault "")
--             , onInput (FieldInput field.ref)
--             , onBlur (FieldEditionStop (Just Valid) field.ref)
--             , Kb.on Kb.Keydown [ ( Kb.Escape, FieldEditionStop Nothing field.ref ) ]
--             ]
--     in
--     div
--         (class "control"
--             :: ifFalse inEdition
--                 [ style "padding-top" ".35em", style "padding-left" ".7em" ]
--                 []
--         )
--         [ strong
--             [ hidden inEdition
--             , classList
--                 [ ( "is-hidden", inEdition )
--                 , ( "has-text-link", not isLocked )
--                 ]
--             , onClick (FieldEditionStart field.ref)
--             , style "padding-top" "calc(.5em - 1px)"
--             ]
--             (textLines field.value)
--         , case field.multiline of
--             Nothing ->
--                 input
--                     (class "input"
--                         :: type_ "text"
--                         :: Kb.on Kb.Keypress [ ( Kb.Enter, FieldEditionStop (Just Valid) field.ref ) ]
--                         :: inputParams
--                     )
--                     []
--             Just nbLines ->
--                 textarea
--                     (class "textarea"
--                         :: rows nbLines
--                         :: inputParams
--                     )
--                     []
--         ]


inputFieldHtml1 : Bool -> Field -> Html Msg
inputFieldHtml1 isLocked field =
    let
        isEmpty =
            field.valueEdited
                |> Maybe.map String.isEmpty
                |> Maybe.withDefault (String.isEmpty field.value)

        inEdition =
            isEmpty || field.valueEdited /= Nothing

        inputParams =
            [ id field.id
            , classList [ ( "is-danger", isEmpty ), ( "is-hidden", not inEdition ) ]
            , hidden (not inEdition)
            , placeholder field.placeholder
            , value (field.valueEdited |> Maybe.withDefault "")
            , onInput (FieldInput field.ref)
            , onBlur (FieldEditionStop (Just Valid) field.ref)
            , Kb.on Kb.Keydown [ ( Kb.Escape, FieldEditionStop Nothing field.ref ) ]
            ]
    in
    div
        (class "control"
            :: ifFalse inEdition
                [ style "padding-top" ".35em", style "padding-left" ".7em" ]
                []
        )
        [ strong
            [ hidden inEdition
            , classList
                [ ( "is-hidden", inEdition )
                , ( "has-text-link", not isLocked )
                ]
            , onClick (FieldEditionStart field.ref)
            , style "padding-top" "calc(.5em - 1px)"
            ]
            (textLines field.value)
        , case field.multiline of
            Nothing ->
                input
                    (class "input"
                        :: type_ "text"
                        :: Kb.on Kb.Keypress [ ( Kb.Enter, FieldEditionStop (Just Valid) field.ref ) ]
                        :: inputParams
                    )
                    []

            Just nbLines ->
                textarea
                    (class "textarea"
                        :: rows nbLines
                        :: inputParams
                    )
                    []
        ]


horizontalLayout1 : String -> Html Msg -> Html Msg
horizontalLayout1 inputLabel inputHtml =
    div []
        [ div [ class "field is-horizontal is-hidden-mobile" ]
            -- [ div [ class "field is-horizontal" ]
            -- [ div [ class "field is-horizontal is-flex-mobile" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label has-text-weight-normal" ]
                    [ inputLabel |> toNonBreakableSpace |> text ]
                ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    -- [ div [ class "control" ]
                    [ inputHtml ]

                -- ]
                ]
            ]
        , div [ class "columns is-mobile is-hidden-tablet" ]
            [ div [ class "column is-3" ]
                [ inputLabel |> toNonBreakableSpace |> text
                ]
            , div [ class "column" ]
                [ inputHtml ]
            ]
        ]


horizontalLayout1b : String -> Html Msg -> Html Msg
horizontalLayout1b inputLabel inputHtml =
    div []
        [ div [ class "field is-horizontal is-hidden-mobile" ]
            -- [ div [ class "field is-horizontal" ]
            -- [ div [ class "field is-horizontal is-flex-mobile" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label has-text-weight-normal" ]
                    [ text (inputLabel ++ "\u{00A0}") ]
                ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    -- [ div [ class "control" ]
                    [ inputHtml ]

                -- ]
                ]
            ]
        , div [ class "columns is-mobile is-hidden-tablet" ]
            [ div [ class "column is-3" ]
                [ text inputLabel
                ]
            , div [ class "column" ]
                [ inputHtml ]
            ]
        ]


checkboxFieldHtml1 : String -> String -> Msg -> Bool -> String -> Html Msg
checkboxFieldHtml1 textSize checkBoxClass toMsg isSelected checkboxText =
    let
        ( iconSize, columnSize ) =
            case textSize of
                "is-size-7" ->
                    ( "", "is-1" )

                _ ->
                    ( "fa-2x", "is-2" )
    in
    div [ class "columns", onClick toMsg ]
        [ div [ class "column has-text-centered", class columnSize, class checkBoxClass ]
            [ span [ class "icon" ]
                [ i
                    [ classList
                        [ ( "far fa-check-square", isSelected )
                        , ( "far fa-square", not isSelected )
                        ]
                    , class iconSize
                    , onClick toMsg
                    ]
                    []
                ]
            ]
        , div [ class "column" ]
            [ p [ class textSize ]
                (textLines checkboxText)
            ]
        ]


checkboxFieldHtml2 : String -> String -> Msg -> Bool -> String -> Html Msg
checkboxFieldHtml2 textSize checkBoxClass toMsg isSelected checkboxText =
    -- let
    --     size =
    --         case textSize of
    --             "is-size-7" ->
    --                 "is-small"
    --             _ ->
    --                 ""
    -- in
    div [ class "field" ]
        [ input
            [ class "is-checkradio"
            , class checkBoxClass
            , class "size"

            -- , id "exampleCheckboxNormal"
            , type_ "checkbox"
            , checked isSelected

            -- , name "exampleCheckboxNormal"
            -- , attrIf isSelected checked True
            , onClick toMsg
            ]
            []
        , label
            [ class textSize
            , onClick toMsg
            , for "exampleCheckboxNormal"
            ]
            (textLines checkboxText)
        ]



-- div [ class "columns is-mobile", onClick toMsg ]
--     [ div [ class "column has-text-centered", class columnSize, class checkBoxClass ]
--         [ span [ class "icon" ]
--             [ i
--                 [ classList
--                     [ ( "far fa-check-square", isSelected )
--                     , ( "far fa-square", not isSelected )
--                     ]
--                 , class iconSize
--                 , onClick toMsg
--                 ]
--                 []
--             ]
--         ]
--     , div [ class "column" ]
--         [ p [ class textSize ]
--             (textLines checkboxText)
--         ]
--     ]


imageFieldHtml : Msg -> Msg -> String -> String -> Html Msg
imageFieldHtml toRequestMsg toRemoveMsg buttonAttrs url =
    if String.isEmpty url then
        -- strong [ class "dont-print has-text-danger", onClick toRequestMsg ]
        --     [ text "Signature" ]
        uploadButton toRequestMsg
            "Charger une signature"
            [ class <| "dont-print " ++ buttonAttrs ]
            "fas fa-camera"
            Nothing

    else
        div [ class "columns is-gapless" ]
            [ div [ class "column is-11" ]
                [ figure [ class "image", onClick toRequestMsg ]
                    [ img [ src url ] []
                    ]
                ]
            , div [ class "column dont-print" ]
                [ button [ class "delete", onClick toRemoveMsg ] []
                ]
            ]


uploadButton : msg -> String -> List (Attribute msg) -> String -> Maybe String -> Html msg
uploadButton toRequestMsg buttonLabel buttonAttrs iconClass maybeComment =
    div
        ([ class "file is-boxed"
         , classList [ ( "has-name", maybeComment /= Nothing ) ]
         ]
            ++ buttonAttrs
        )
        [ label [ class "file-label" ]
            [ input [ class "file-input", type_ "file", name "resume", onClick toRequestMsg ]
                []
            , span [ class "file-cta" ]
                [ span [ class "file-icon" ]
                    -- [ i [ class "fas fa-upload" ]
                    [ i [ class iconClass ]
                        []
                    ]
                , span [ class "file-label" ]
                    [ text buttonLabel ]
                ]
            , case maybeComment of
                Just comment ->
                    span [ class "file-name" ]
                        [ text comment ]

                Nothing ->
                    text ""
            ]
        ]



-- checkboxFieldHtml : String -> Bool -> ( Int, String ) -> Html Msg
-- checkboxFieldHtml checkBoxClass isSelected ( fieldNum, checkboxText ) =
--     div [ class "columns is-mobile", onClick (MotivationCheckboxToggle fieldNum) ]
--         [ div [ class "column is-2 has-text-centered", class checkBoxClass ]
--             [ i
--                 [ classList
--                     [ ( "far fa-2x fa-check-square", isSelected )
--                     , ( "far fa-2x fa-square", not isSelected )
--                     ]
--                 ]
--                 []
--             ]
--         , div [ class "column" ]
--             [ p []
--                 (textLines checkboxText)
--             ]
--         ]


viewDebug : Model -> Html Msg
viewDebug model =
    if debug then
        p []
            [ ol [] (model.error |> List.map (D.errorToString >> text >> List.singleton >> li []))
            ]

    else
        text ""


helpButton : Model -> Html Msg
helpButton model =
    span
        [ class "icon has-text-info"

        -- , classList [ ( "has-text-success", not model.locked ) ]
        , onClick HelpDisplayStatusToggle
        ]
        [ i [ class "fas fa-2x fa-info-circle" ]
            -- [ i [ class "fas fa-2x fa-question-circle" ]
            []
        ]


clearFormButton : Model -> Html Msg
clearFormButton model =
    span
        [ class "icon"
        , classList
            [ ( "has-text-grey-lighter", formIsValidated model )
            , ( "has-text-success", not <| formIsValidated model )
            ]
        , onClick ClearForm
        ]
        [ i [ class "fas fa-2x fa-eraser" ]
            []
        ]


printButton : Model -> Html Msg
printButton model =
    span
        [ class "icon"
        , classList
            [ ( "has-text-success", formIsValidated model )
            , ( "has-text-grey-lighter", not <| formIsValidated model )
            ]
        , onClick PrintRequested
        ]
        [ i
            [ class "fas fa-2x fa-print"
            , classList [ ( "blinking", model.printingStarting ) ]
            ]
            []
        ]


stateButton : Model -> Html Msg
stateButton model =
    let
        colorClass =
            if formIsImcomplete model then
                "has-text-danger"

            else if not <| formIsValidated model then
                "has-text-warning"

            else
                ""
    in
    span
        [ class "icon"
        , class colorClass

        -- , onDoubleClick StateButtonToggle
        , onClick StateButtonToggle
        ]
        [ i
            [ class "fa-2x"
            , classList
                [ ( "fas fa-check-square", not <| formIsValidated model )
                , ( "fas fa-edit", formIsValidated model )
                ]
            ]
            []
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div [ class "level is-mobile" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ] [ stateButton model ]
            , div [ class "level-item" ] [ printButton model ]
            , div [ class "level-item" ] [ clearFormButton model ]
            ]
        , div [ class "level-right" ] [ helpButton model ]
        ]


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ helpModal model
            , div [ class "columns", classList [ ( "is-clipped", model.helpDisplayed ) ] ]
                [ div [ class "column is-8 is-offset-2" ]
                    [ viewDebug model

                    -- , viewAlert
                    , viewToolbar model
                    , viewFormV2 model
                    , viewToolbar model
                    ]
                ]
            ]
        ]


viewAlert : Html msg
viewAlert =
    div [ class "message is-primary" ]
        [ div [ class "message-body" ]
            [ text "ATTENTION : Pour être valable cette attestation doit obligatoirement imprimée, datée du jour et signée."
            ]
        ]


viewFormV2 : Model -> Html Msg
viewFormV2 model =
    let
        size =
            "7"

        textSize =
            ifTrue (String.isEmpty size) "" ("is-size-" ++ size)

        -- p_ attrs =
        --     p (class textSize :: attrs)
    in
    div [ id printId, class "content has-text-justified is-size-7" ]
        [ h2 [ class "has-text-centered" ]
            [ text "ATTESTATION DE DÉPLACEMENT DÉROGATOIRE"
            ]

        -- , ifTrue model.printingStarting (h4 [] [ text "(printingStarting)" ]) (text "")
        , p [ class "has-text-centered" ]
            [ text """En application de l’article 3 du décret du 23 mars 2020 prescrivant les mesures générales
            nécessaires pour faire face à l’épidémie de Covid19 dans le cadre de l’état d’urgence sanitaire""" ]

        -- , br [ class "dont-print" ] []
        , p []
            [ text "Je soussigné(e)," ]
        , horizontalInputField2 (formIsValidated model) "Prénom :" model.firstname
        , horizontalInputField2 (formIsValidated model) "Nom :" model.lastname
        , horizontalInputField2 (formIsValidated model) "Né(e) le :" model.birthdate
        , horizontalInputField2 (formIsValidated model) "À :" model.birthplace
        , horizontalInputField2 (formIsValidated model) "Adresse :" model.address
        , horizontalInputField2 (formIsValidated model) "Ville :" model.town
        , horizontalInputField2 (formIsValidated model) "Code postal :" model.zipcode

        -- , horizontalLayout1 "Mme / M. :" (inputFieldHtml1 (formIsValidated model) model.name)
        -- , horizontalLayout1 "Né(e) le :" (inputFieldHtml1 (formIsValidated model) model.birthdate)
        -- , horizontalLayout1 "À :" (inputFieldHtml1 (formIsValidated model) model.birthplace)
        -- , horizontalLayout1 "Demeurant :" (inputFieldHtml1 (formIsValidated model) model.address)
        , br [] []
        , p []
            [ text """certifie que mon déplacement est lié au motif suivant (cocher la case) autorisé par l’article 3 du
            décret du 23 mars 2020 prescrivant les mesures générales nécessaires pour faire face à
            l’épidémie de Covid19 dans le cadre de l’état d’urgence sanitaire(1) :""" ]
        , div []
            (let
                checkboxClass =
                    if noneMotivationIsChosen model then
                        "has-text-danger"

                    else if not (formIsValidated model) then
                        "has-text-link"

                    else
                        ""
             in
             listIndexedMap2
                (\i checked desc ->
                    checkboxFieldHtml2 textSize checkboxClass (MotivationCheckboxToggle i) checked desc
                )
                model.motivations
                motivationsDescription
            )

        -- , br [] []
        , br [] []
        , div [ class "columns" ]
            [ div [ class "column is-6" ]
                -- [ horizontalLayout1 "Fait à :" <| inputFieldHtml1 (formIsValidated model) model.place ]
                [ horizontalInputField2 (formIsValidated model) "Fait à :" model.place ]
            ]
        , div [ class "columns" ]
            [ div [ class "column is-3" ]
                -- [ horizontalLayout1 "le : " <| inputFieldHtml1 (formIsValidated model) model.date ]
                [ horizontalInputField2 (formIsValidated model) "Le :" model.date ]
            , div [ class "column is-3" ]
                -- [ horizontalLayout1 "à " <| inputFieldHtml1 (formIsValidated model) model.outingTime ]
                [ horizontalInputField2 (formIsValidated model) "à" model.outingTime ]
            ]
        , div [ class "columns" ]
            -- [ div [ class "column is-offset-5 is-7 has-text-centered" ]
            [ div [ class "column is-2" ]
                -- [ text "Signature" ]
                [ if formIsValidated model then
                    qrCodeView model

                  else
                    text ""
                ]

            -- , div
            --     [ class "column is-5 dont-print"
            --     -- , class "is-flex"
            --     -- , style "justify-content" "center"
            --     ]
            --     -- [ imageFieldHtml SignatureRequested SignatureRemoveRequested "is-info" model.signature
            --     [ if String.isEmpty model.signature then
            --         uploadButton SignatureRequested
            --             "Charger une signature"
            --             [ class "dont-print"
            --             , classList
            --                 [ ( "is-info", not (formIsValidated model) )
            --                 , ( "is-invisible", (formIsValidated model) )
            --                 ]
            --             ]
            --             "fas fa-camera"
            --             Nothing
            --       else if (formIsValidated model) then
            --         figure [ class "image" ]
            --             [ img [ src model.signature ] []
            --             ]
            --       else
            --         div [ class "notification is-info" ]
            --             [ figure [ class "image is-marginless", onClick SignatureRequested ]
            --                 [ img [ src model.signature ] []
            --                 ]
            --             , button
            --                 [ class "delete"
            --                 , classList
            --                     [ ( "is-hidden", (formIsValidated model) )
            --                     ]
            --                 , onClick SignatureRemoveRequested
            --                 ]
            --                 []
            --             ]
            --     ]
            ]
        , p []
            (textLines
                """(1) Les personnes souhaitant bénéficier de l'une de ces exceptions doivent se munir s'il y a lieu, lors de leurs déplacements hors de leur domicile, d'un document leur permettant de justifier que le déplacement considéré entre dans le champ de l'une de ces exceptions.
        (2) A utiliser par les travailleurs non-salariés, lorsqu’ils ne peuvent disposer d’un justificatif de déplacement établi par leur employeur.
        (3) Y compris les acquisitions à titre gratuit (distribution de denrées alimentaires…) et les déplacements liés à la perception de prestations sociales et au retrait d’espèces.
        """
            )
        ]
