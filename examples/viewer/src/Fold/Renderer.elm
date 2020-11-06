module Fold.Renderer exposing (..)

import Circle2d
import Fold.Vertex as Vertex exposing (Vertex)
import Geometry.Svg
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


{-| -}
vertex : Vertex Pixels coordinates -> Svg msg
vertex theVertex =
    Circle2d.atPoint (Vertex.coordinate theVertex) (Pixels.float 4)
        |> Geometry.Svg.circle2d
            [ Attributes.strokeWidth "3px"
            , Attributes.stroke "black"
            , Attributes.fill "none"
            ]


{-| -}
edge : LineSegment2d Pixels coordinates -> Svg msg
edge lineSegment =
    Geometry.Svg.lineSegment2d
        [ Attributes.strokeWidth "2px"
        , Attributes.stroke "gray"
        ]
        lineSegment
