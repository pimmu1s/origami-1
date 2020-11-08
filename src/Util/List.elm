module Util.List exposing (..)

{-| -}


{-| If a list is empty. Fill the list to make it a list of empty lists of a
particular length.
-}
fillEmpty : List (List a) -> Int -> List (List a)
fillEmpty list length =
    if List.length list == length then
        list

    else
        List.repeat length []
