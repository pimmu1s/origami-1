module Fold.Types exposing (Unit(..), unitToString, unitFromString)

{-|


# Unit

@docs Unit, unitToString, unitFromString

-}


type Unit
    = Unit
    | Inches
    | Point
    | Meters
    | Centimeters
    | Millimeters
    | Microns
    | Nanometers


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
