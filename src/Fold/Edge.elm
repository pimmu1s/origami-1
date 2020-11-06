module Fold.Edge exposing
    ( Edge, EdgeType
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

import Angle exposing (Angle)
import Fold.Internal as Internal



-- Type


type alias Edge =
    Internal.Edge


type alias EdgeType =
    Internal.EdgeType



-- Builders


{-| -}
edgeType : Edge -> EdgeType
edgeType (Internal.Edge properties) =
    properties.edgeType


{-| -}
foldAngle : Edge -> Angle
foldAngle (Internal.Edge properties) =
    properties.foldAngle



-- Modifiers


{-| -}
setEdgeType : EdgeType -> Edge -> Edge
setEdgeType newEdgeType (Internal.Edge properties) =
    Internal.Edge { properties | edgeType = newEdgeType }


{-| -}
setFoldAngle : Angle -> Edge -> Edge
setFoldAngle newFoldAngle (Internal.Edge properties) =
    Internal.Edge { properties | foldAngle = newFoldAngle }
