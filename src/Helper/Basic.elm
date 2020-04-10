module Helper.Basic exposing (..)

---- STRING ----


toNonBreakableSpace : String -> String
toNonBreakableSpace str =
    String.replace " " "\u{00A0}" str



---- TERNARY OPERATOR ----


ifTrue : Bool -> a -> a -> a
ifTrue test result elseResult =
    if test then
        result

    else
        elseResult


ifFalse : Bool -> a -> a -> a
ifFalse test =
    ifTrue (not test)



---- LIST ----


listIndexedMap2 : (Int -> a -> b -> c) -> List a -> List b -> List c
listIndexedMap2 fn la lb =
    List.map2 Tuple.pair la lb
        |> List.indexedMap (\i ( a, b ) -> fn i a b)


listFilterIndexedMap2 : (Int -> a -> b -> Maybe c) -> List a -> List b -> List c
listFilterIndexedMap2 fn la lb =
    List.map2 Tuple.pair la lb
        |> List.indexedMap (\i ( a, b ) -> fn i a b)
        |> List.filterMap identity
