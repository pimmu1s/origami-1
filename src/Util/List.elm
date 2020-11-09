module Util.List exposing (..)

{-| -}


{-| If a list is empty. Fill the list to make it a list of empty lists of a
particular length.
-}
fillEmpty : Int -> a -> List a -> List a
fillEmpty length default list =
    if List.length list == length then
        list

    else
        List.repeat length default
