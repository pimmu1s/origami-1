module Fold.Internal exposing
    ( Vertex(..), Edge(..), Face(..), Id(..)
    , EdgeType(..)
    )

{-|


# Types

@docs Vertex, Edge, Face, Id

-}

import Angle exposing (Angle)
import Point2d exposing (Point2d)


{-| -}
type Vertex units coordinates
    = Vertex
        { coordinate : Point2d units coordinates
        , id : Id
        }


{-| -}
type Face
    = Face
        { vertices : ( Int, Int, Int )
        , edges : ( Int, Int, Int )
        , id : Id
        }


{-| -}
type Edge
    = Edge
        { edgeType : EdgeType
        , foldAngle : Angle
        , id : Id
        }


{-| -}
type EdgeType
    = Boundary
    | Mountain
    | Valley
    | Flat
    | Unassigned


{-| -}
type Id
    = Id Int
