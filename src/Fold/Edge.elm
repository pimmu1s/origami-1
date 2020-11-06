module Fold.Edge exposing
    ( Edge
    , edgeType, foldAngle
    , setEdgeType, setFoldAngle
    )

{-|


# Types

@docs Edge


# Accessors

@docs edgeType, foldAngle


# Modifiers

@docs setEdgeType, setFoldAngle

-}

import Angle exposing (Angle)
import Fold.Internal as Internal exposing (Edge(..), Face, Vertex(..))
import Fold.Types exposing (EdgeType)



-- Type


{-| -}
type alias Edge =
    Internal.Edge



-- Accessors


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
