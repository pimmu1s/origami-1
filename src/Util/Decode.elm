module Util.Decode exposing (maybeInt, maybeString, maybeList, maybeWithDefault)

{-|


# Maybe

@docs maybeInt, maybeString, maybeList, maybeWithDefault

-}

import Json.Decode as Decode exposing (Decoder)


{-| Must specify the default integer value.
-}
maybeInt : String -> Int -> Decoder Int
maybeInt field default =
    maybeWithDefault field Decode.int default


{-| Default to the empty string `""`.
-}
maybeString : String -> Decoder String
maybeString field =
    maybeWithDefault field Decode.string ""


{-| Default to the empty list `[]`. Must specify the list decoder.
-}
maybeList : String -> Decoder a -> Decoder (List a)
maybeList field theDecoder =
    maybeWithDefault field (Decode.list theDecoder) []


{-| Create a decoder that defaults to a particular value.
-}
maybeWithDefault : String -> Decoder a -> a -> Decoder a
maybeWithDefault field decoder default =
    Decode.field field decoder
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault default)
