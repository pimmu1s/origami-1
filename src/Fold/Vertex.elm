module Fold.Vertex exposing
    ( Vertex
    , coordinate, setCoordinate
    )

{-|


# Types

@docs Vertex


# Accessors

@docs coordinates


# Modifiers

@docs setCoordinates


# Json

@docs encode, decoder

-}

import Fold.Internal as Internal
import Point2d exposing (Point2d)



-- Types


type alias Vertex units coordinates =
    Internal.Vertex units coordinates



-- Accessors


{-| -}
coordinate : Vertex units coordinates -> Point2d units coordinates
coordinate (Internal.Vertex properties) =
    properties.coordinate



-- Modifiers


{-| -}
setCoordinate : Point2d units coordinates -> Vertex units coordinates -> Vertex units coordinates
setCoordinate newCoordinates (Internal.Vertex properties) =
    Internal.Vertex { properties | coordinate = newCoordinates }
