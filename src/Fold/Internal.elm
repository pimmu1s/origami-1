module Fold.Internal exposing (Vertex(..), Edge(..), Face(..))

{-|


# Types

@docs Vertex, Edge, Face, Id

-}

import Angle exposing (Angle)
import Fold.Types exposing (EdgeType)
import Point2d exposing (Point2d)


{-| -}
type Vertex units coordinates
    = Vertex
        { coordinate : Point2d units coordinates
        , adjacency : List Int
        , faces : List Int
        , id : Int
        }


{-| -}
type Edge
    = Edge
        { startVertex : Int
        , endVertex : Int
        , faces : List Int
        , edgeType : EdgeType
        , foldAngle : Angle
        , id : Int
        }


{-| -}
type Face
    = Face
        { vertices : List Int
        , edges : List Int
        , id : Int
        }
