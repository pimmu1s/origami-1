module Util.String exposing (..)

{-| -}


{-|

    surround "(" ")" "x, y"
        --> "(x, y)"

-}
surround : String -> String -> String -> String
surround left right string =
    left ++ string ++ right
