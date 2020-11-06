module Fold.Frame exposing
    ( Frame, Class(..), Attribute(..)
    , empty
    , author, title, description, classes, attributes, unit
    , setAuthor, setTitle, setDescription, setClasses, setAttributes, setUnit
    , encode, decoder
    )

{-|


# Types

@docs Frame, Class, Attribute


# Builder

@docs empty


# Metadata


## Accessors

@docs author, title, description, classes, attributes, unit


## Modifiers

@docs setAuthor, setTitle, setDescription, setClasses, setAttributes, setUnit


# Json

@docs encode, decoder

-}

import Angle exposing (Angle)
import Fold.Edge exposing (EdgeType(..))
import Fold.Types as Type exposing (Unit(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Point2d exposing (Point2d)
import Quantity


{-| -}
type Frame units coordinates
    = Frame Properties (Vertices units coordinates) Edges Faces


{-| -}
type alias Properties =
    { author : String
    , title : String
    , description : String
    , classes : List Class
    , attributes : List Attribute
    , unit : Unit
    }


type alias Vertices units coordinates =
    { vertices : List (Point2d units coordinates)
    , adjacency : List (List Int)
    , faces : List (List Int)
    }


type alias Edges =
    { vertices : List (List Int)
    , faces : List (List Int)
    , assignment : List EdgeType
    , foldAngle : List Angle
    }


type alias Faces =
    { vertices : List (List Int)
    , edges : List (List Int)
    }


{-| -}
type Class
    = CreasePattern
    | FoldedForm
    | Graph
    | Linkage


{-| -}
type Attribute
    = Dimension2d
    | Dimension3d
    | Abstract
    | Manifold
    | NonManifold
    | Orientable
    | SelfTouching
    | NonSelfTouching
    | SelfIntersecting
    | NonSelfInteresting



-- Builders


{-| -}
empty : Frame units coordinates
empty =
    Frame
        { author = ""
        , title = ""
        , description = ""
        , classes = []
        , attributes = []
        , unit = Unit
        }
        { vertices = []
        , adjacency = []
        , faces = []
        }
        { vertices = []
        , faces = []
        , assignment = []
        , foldAngle = []
        }
        { vertices = []
        , edges = []
        }



-- Metadata
-- Accessors


{-| -}
author : Frame units coordinates -> String
author (Frame properties _ _ _) =
    properties.author


{-| -}
title : Frame units coordinates -> String
title (Frame properties _ _ _) =
    properties.title


{-| -}
description : Frame units coordinates -> String
description (Frame properties _ _ _) =
    properties.description


{-| -}
classes : Frame units coordinates -> List Class
classes (Frame properties _ _ _) =
    properties.classes


{-| -}
attributes : Frame units coordinates -> List Attribute
attributes (Frame properties _ _ _) =
    properties.attributes


{-| -}
unit : Frame units coordinates -> Unit
unit (Frame properties _ _ _) =
    properties.unit



-- Modifiers


theProperties : Frame units coordinates -> Properties
theProperties (Frame properties _ _ _) =
    properties


setProperties : Properties -> Frame units coordinates -> Frame units coordinates
setProperties properties (Frame _ vertices edges faces) =
    Frame properties vertices edges faces


{-| -}
setAuthor : String -> Frame units coordinates -> Frame units coordinates
setAuthor newAuthor frame =
    let
        properties =
            theProperties frame
    in
    setProperties { properties | author = newAuthor } frame


{-| -}
setTitle : String -> Frame units coordinates -> Frame units coordinates
setTitle newTitle frame =
    let
        properties =
            theProperties frame
    in
    setProperties { properties | title = newTitle } frame


{-| -}
setDescription : String -> Frame units coordinates -> Frame units coordinates
setDescription newDescription frame =
    let
        properties =
            theProperties frame
    in
    setProperties { properties | description = newDescription } frame


{-| -}
setClasses : List Class -> Frame units coordinates -> Frame units coordinates
setClasses newClasses frame =
    let
        properties =
            theProperties frame
    in
    setProperties { properties | classes = newClasses } frame


{-| -}
setAttributes : List Attribute -> Frame units coordinates -> Frame units coordinates
setAttributes newAttributes frame =
    let
        properties =
            theProperties frame
    in
    setProperties { properties | attributes = newAttributes } frame


{-| -}
setUnit : Unit -> Frame units coordinates -> Frame units coordinates
setUnit newUnit frame =
    let
        properties =
            theProperties frame
    in
    setProperties { properties | unit = newUnit } frame



-- Json
-- Encode


{-| -}
encode : Frame units coordinates -> Value
encode (Frame properties vertices edges faces) =
    Encode.object <|
        encodeProperties properties
            ++ encodeVertices vertices
            ++ encodeEdges edges
            ++ encodeFaces faces


encodeProperties : Properties -> List ( String, Value )
encodeProperties properties =
    let
        encodeClass =
            \class ->
                Encode.string <|
                    case class of
                        CreasePattern ->
                            "creasePattern"

                        FoldedForm ->
                            "foldedForm"

                        Graph ->
                            "graph"

                        Linkage ->
                            "linkage"

        encodeAttribute =
            \attribute ->
                Encode.string <|
                    case attribute of
                        Dimension2d ->
                            "2D"

                        Dimension3d ->
                            "3D"

                        Abstract ->
                            "abstract"

                        Manifold ->
                            "manifold"

                        NonManifold ->
                            "nonManifold"

                        Orientable ->
                            "orientable"

                        SelfTouching ->
                            "nonOrientable"

                        NonSelfTouching ->
                            "selfTouching"

                        SelfIntersecting ->
                            "nonSelfTouching"

                        NonSelfInteresting ->
                            "nonSelfIntersecting"
    in
    [ ( "frame_author", Encode.string properties.author )
    , ( "frame_title", Encode.string properties.title )
    , ( "frame_description", Encode.string properties.description )
    , ( "frame_classes", Encode.list encodeClass properties.classes )
    , ( "frame_attributes", Encode.list encodeAttribute properties.attributes )
    , ( "frame_unit", Encode.string (Type.unitToString properties.unit) )
    ]


encodeIds : List (List Int) -> Value
encodeIds =
    Encode.list <| Encode.list Encode.int


encodeVertices : Vertices units coordinates -> List ( String, Value )
encodeVertices vertices =
    let
        encodePoints =
            Encode.list
                (\vertex ->
                    Encode.list Encode.float
                        [ Quantity.unwrap <| Point2d.xCoordinate vertex
                        , Quantity.unwrap <| Point2d.yCoordinate vertex
                        ]
                )
    in
    [ ( "vertices_coords", encodePoints vertices.vertices )
    , ( "vertices_vertices", encodeIds vertices.adjacency )
    , ( "vertices_faces", encodeIds vertices.faces )
    ]


encodeEdges : Edges -> List ( String, Value )
encodeEdges edges =
    let
        encodeEdgeType =
            \string ->
                Encode.string <|
                    case string of
                        Boundary ->
                            "B"

                        Mountain ->
                            "M"

                        Valley ->
                            "V"

                        Flat ->
                            "F"

                        Unassigned ->
                            "U"

        encodeAngle =
            Encode.float << Angle.inDegrees
    in
    [ ( "edges_vertices", encodeIds edges.vertices )
    , ( "edges_faces", encodeIds edges.faces )
    , ( "edges_assignment", Encode.list encodeEdgeType edges.assignment )
    , ( "edges_foldAngle", Encode.list encodeAngle edges.foldAngle )
    ]


encodeFaces : Faces -> List ( String, Value )
encodeFaces faces =
    [ ( "faces_vertices", encodeIds faces.vertices )
    , ( "faces_edges", encodeIds faces.edges )
    ]



-- Decoder


{-| -}
decoder : Decoder (Frame units coordinates)
decoder =
    Decode.map4 Frame
        decoderProperties
        decoderVertices
        decoderEdges
        decoderFaces


decoderProperties : Decoder Properties
decoderProperties =
    let
        decodeClass =
            Decode.string
                |> Decode.andThen
                    (\string ->
                        case string of
                            "creasePattern" ->
                                Decode.succeed CreasePattern

                            "foldedForm" ->
                                Decode.succeed FoldedForm

                            "graph" ->
                                Decode.succeed Graph

                            "linkage" ->
                                Decode.succeed Linkage

                            _ ->
                                Decode.fail <| "\"" ++ string ++ "\" is not a valid frame class."
                    )

        decodeAttribute =
            Decode.string
                |> Decode.andThen
                    (\string ->
                        case string of
                            "2D" ->
                                Decode.succeed Dimension2d

                            "3D" ->
                                Decode.succeed Dimension3d

                            "abstract" ->
                                Decode.succeed Abstract

                            "manifold" ->
                                Decode.succeed Manifold

                            "nonManifold" ->
                                Decode.succeed NonManifold

                            "orientable" ->
                                Decode.succeed Orientable

                            "nonOrientable" ->
                                Decode.succeed SelfTouching

                            "selfTouching" ->
                                Decode.succeed NonSelfTouching

                            "nonSelfTouching" ->
                                Decode.succeed SelfIntersecting

                            "nonSelfIntersecting" ->
                                Decode.succeed NonSelfInteresting

                            _ ->
                                Decode.fail <| "\"" ++ string ++ "\" is not a valid frame attribute."
                    )

        decodeUnit =
            Decode.string
                |> Decode.andThen
                    (\string ->
                        case Type.unitFromString string of
                            Just theUnit ->
                                Decode.succeed theUnit

                            Nothing ->
                                Decode.fail <| "\"" ++ string ++ "\" is not a valid frame unit value."
                    )
    in
    Decode.map6 Properties
        (Decode.field "frame_author" Decode.string)
        (Decode.field "frame_title" Decode.string)
        (Decode.field "frame_description" Decode.string)
        (Decode.field "frame_classes" <| Decode.list decodeClass)
        (Decode.field "frame_attributes" <| Decode.list decodeAttribute)
        (Decode.field "frame_unit" decodeUnit)


decodeIds : Decoder (List (List Int))
decodeIds =
    Decode.list <| Decode.list Decode.int


decoderVertices : Decoder (Vertices units coordinates)
decoderVertices =
    let
        decoderPoint2d =
            Decode.list Decode.float
                |> Decode.andThen
                    (\floatList ->
                        case floatList of
                            x :: y :: _ ->
                                Point2d.xy
                                    (Quantity.unsafe x)
                                    (Quantity.unsafe y)
                                    |> Decode.succeed

                            _ ->
                                Decode.fail "Vertex array must be of length 2 or greater"
                    )
    in
    Decode.map3 Vertices
        (Decode.field "vertices_coords" <| Decode.list decoderPoint2d)
        (Decode.field "vertices_vertices" decodeIds)
        (Decode.field "vertices_faces" decodeIds)


decoderEdges : Decoder Edges
decoderEdges =
    let
        decoderEdgeType =
            Decode.string
                |> Decode.andThen
                    (\string ->
                        case string of
                            "B" ->
                                Decode.succeed Boundary

                            "M" ->
                                Decode.succeed Mountain

                            "V" ->
                                Decode.succeed Valley

                            "F" ->
                                Decode.succeed Flat

                            "U" ->
                                Decode.succeed Unassigned

                            _ ->
                                Decode.fail <| "\"" ++ string ++ "\" is not a valid edge fold type."
                    )

        decoderAngle =
            Decode.map Angle.degrees Decode.float
    in
    Decode.map4 Edges
        (Decode.field "edges_vertices" decodeIds)
        (Decode.field "edges_faces" decodeIds)
        (Decode.field "edges_assignment" <| Decode.list decoderEdgeType)
        (Decode.field "edges_foldAngle" <| Decode.list decoderAngle)


decoderFaces : Decoder Faces
decoderFaces =
    Decode.map2 Faces
        (Decode.field "faces_vertices" decodeIds)
        (Decode.field "faces_edges" decodeIds)
