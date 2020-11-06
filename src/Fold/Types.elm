module Fold.Types exposing
    ( EdgeType(..)
    , Unit(..), unitToString, unitFromString
    )

{-|


# Edge

@docs EdgeType


# Unit

@docs Unit, unitToString, unitFromString

-}


{-| -}
type EdgeType
    = Boundary
    | Mountain
    | Valley
    | Flat
    | Unassigned


{-| -}
type Unit
    = Unit
    | Inches
    | Point
    | Meters
    | Centimeters
    | Millimeters
    | Microns
    | Nanometers


{-| -}
unitToString : Unit -> String
unitToString unit =
    case unit of
        Unit ->
            "unit"

        Inches ->
            "in"

        Point ->
            "pt"

        Meters ->
            "m"

        Centimeters ->
            "cm"

        Millimeters ->
            "mm"

        Microns ->
            "um"

        Nanometers ->
            "nm"


{-| -}
unitFromString : String -> Maybe Unit
unitFromString string =
    case string of
        "unit" ->
            Just Unit

        "in" ->
            Just Inches

        "pt" ->
            Just Point

        "m" ->
            Just Meters

        "cm" ->
            Just Centimeters

        "mm" ->
            Just Millimeters

        "um" ->
            Just Microns

        "nm" ->
            Just Nanometers

        _ ->
            Nothing
