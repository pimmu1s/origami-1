module Fold.Edge exposing
    ( Edge, EdgeType(..)
    , edgeType, foldAngle
    , setEdgeType, setFoldAngle
    )

{-|


# Types

@docs Edge, EdgeType


# Accessors

@docs edgeType, foldAngle


# Modifiers

@docs setEdgeType, setFoldAngle

-}

-- Type

import Angle exposing (Angle)


{-| -}
type Edge
    = Edge
        { edgeType : EdgeType
        , foldAngle : Angle
        }


{-| -}
type EdgeType
    = Boundry
    | Mountain
    | Valey
    | Flat
    | Unassigned



-- Builders


{-| -}
edgeType : Edge -> EdgeType
edgeType (Edge properties) =
    properties.edgeType


{-| -}
foldAngle : Edge -> Angle
foldAngle (Edge properties) =
    properties.foldAngle



-- Modifiers


{-| -}
setEdgeType : EdgeType -> Edge -> Edge
setEdgeType newEdgeType (Edge properties) =
    Edge { properties | edgeType = newEdgeType }


{-| -}
setFoldAngle : Angle -> Edge -> Edge
setFoldAngle newFoldAngle (Edge properties) =
    Edge { properties | foldAngle = newFoldAngle }
