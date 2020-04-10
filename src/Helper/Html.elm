module Helper.Html exposing (..)

import Helper.Basic exposing (ifTrue)
import Html exposing (Attribute)
import Html.Attributes exposing (class, style)


addAttrList : List ( Attribute msg, Bool ) -> List (Attribute msg) -> List (Attribute msg)
addAttrList attrList attrs =
    attrs
        ++ (attrList
                |> List.filterMap
                    (\( attr, cond ) ->
                        ifTrue cond (Just attr) Nothing
                    )
           )


attrIf : Bool -> Attribute msg -> Attribute msg
attrIf cond attr =
    ifTrue cond attr (class "")


styleIf : Bool -> String -> String -> Attribute msg
styleIf cond name value =
    if cond then
        style name value

    else
        class ""
