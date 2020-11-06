module Fold.Vertex exposing
    ( Vertex
    , at
    , encode, decoder
    , coordinate, setCoordinate
    )

{-|


# Types

@docs Vertex


# Builder

@docs at


# Accessors

@docs coordinates


# Modifiers

@docs setCoordinates


# Json

@docs encode, decoder

-}

import Fold.Types exposing (Unit(..))
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Point2d exposing (Point2d)
import Quantity



-- Types


{-| -}
type Vertex units coordinates
    = Vertex { coordinate : Point2d units coordinates }



-- Builders


{-| -}
at : Point2d units coordinates -> Vertex units coordinates
at point =
    Vertex { coordinate = point }



-- Accessors


{-| -}
coordinate : Vertex units coordinates -> Point2d units coordinates
coordinate (Vertex properties) =
    properties.coordinate



-- Modifiers


{-| -}
setCoordinate : Point2d units coordinates -> Vertex units coordinates -> Vertex units coordinates
setCoordinate newCoordinates (Vertex properties) =
    Vertex { properties | coordinate = newCoordinates }



-- Json


{-| -}
encode : Vertex units coordinates -> Value
encode (Vertex properties) =
    Encode.list Encode.float
        [ Point2d.xCoordinate properties.coordinate |> Quantity.unwrap
        , Point2d.yCoordinate properties.coordinate |> Quantity.unwrap
        ]


{-| -}
decoder : Decode.Decoder (Vertex units coordinates)
decoder =
    Decode.list Decode.float
        |> Decode.andThen
            (\floatList ->
                case floatList of
                    x :: y :: _ ->
                        at
                            (Point2d.xy
                                (Quantity.unsafe x)
                                (Quantity.unsafe y)
                            )
                            |> Decode.succeed

                    _ ->
                        Decode.fail "Vertex array must be of length 2 or greater"
            )
