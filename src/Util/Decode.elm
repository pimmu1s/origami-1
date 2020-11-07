module Util.Decode exposing (maybeInt, maybeString, maybeList)

{-|


# Maybe

@docs maybeInt, maybeString, maybeList

-}

import Json.Decode as Decode exposing (Decoder)


{-| Must specify the default integer value.
-}
maybeInt : String -> Int -> Decoder Int
maybeInt name default =
    Decode.field name Decode.int
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault default)


{-| Default to the empty string `""`.
-}
maybeString : String -> Decoder String
maybeString name =
    Decode.field name Decode.string
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault "")


{-| Default to the empty list `[]`. Must specify the list decoder.
-}
maybeList : String -> Decoder a -> Decoder (List a)
maybeList name theDecoder =
    Decode.list theDecoder
        |> Decode.field name
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault [])
