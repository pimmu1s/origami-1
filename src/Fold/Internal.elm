module Fold.Internal exposing (Face(..))

{-|


# Types

@docs Face

-}


{-| -}
type Face
    = Face
        { vertices : ( Int, Int, Int )
        , edges : ( Int, Int, Int )
        }
