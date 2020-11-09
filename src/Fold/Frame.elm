module Fold.Frame exposing
    ( Frame, Class(..), Attribute(..)
    , empty, with, header
    , vertices, vertexAdjacencies, vertexFaces
    , edges, edgeStartVertex, edgeEndVertex, edgeVertices, edgeFaces
    , faces, faceEdges, faceVertices
    , author, title, description, classes, attributes, unit
    , setAuthor, setTitle, setDescription, setClasses, setAttributes, setUnit
    , encode, encodePartial, decoder
    )

{-|


# Types

@docs Frame, Class, Attribute


# Builder

@docs empty, with, header


# Vertices

@docs vertices, vertexAdjacencies, vertexFaces


# Edges

@docs edges, edgeStartVertex, edgeEndVertex, edgeVertices, edgeFaces


# Faces

@docs faces, faceEdges, faceVertices


# Metadata


## Accessors

@docs author, title, description, classes, attributes, unit


## Modifiers

@docs setAuthor, setTitle, setDescription, setClasses, setAttributes, setUnit


# Json

@docs encode, encodePartial, decoder

-}

import Angle exposing (Angle)
import Dict exposing (Dict)
import Fold.Internal exposing (Edge(..), Face(..), Vertex(..))
import Fold.Types as Type exposing (EdgeType(..), Unit(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Point2d exposing (Point2d)
import Quantity
import Util.Decode as Decode
import Util.List as List


{-| -}
type Frame units coordinates
    = Frame Metadata (Body units coordinates)


{-| -}
type alias Metadata =
    { author : String
    , title : String
    , description : String
    , classes : List Class
    , attributes : List Attribute
    , unit : Unit
    }


type alias Body units coordinates =
    { vertices : Dict Int (Vertex units coordinates)
    , edges : Dict Int Edge
    , faces : Dict Int Face
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
    with
        { author = ""
        , title = ""
        , description = ""
        , classes = []
        , attributes = []
        , unit = Unit
        }
        { vertices = Dict.empty
        , edges = Dict.empty
        , faces = Dict.empty
        }


{-| -}
with : Metadata -> Body units coordinates -> Frame units coordinates
with =
    Frame


{-| -}
header :
    { author : String
    , title : String
    , description : String
    , classes : List Class
    , attributes : List Attribute
    , unit : Unit
    }
    -> Metadata
header =
    identity



-- Metadata
-- Accessors


{-| -}
author : Frame units coordinates -> String
author (Frame metadata _) =
    metadata.author


{-| -}
title : Frame units coordinates -> String
title (Frame metadata _) =
    metadata.title


{-| -}
description : Frame units coordinates -> String
description (Frame metadata _) =
    metadata.description


{-| -}
classes : Frame units coordinates -> List Class
classes (Frame metadata _) =
    metadata.classes


{-| -}
attributes : Frame units coordinates -> List Attribute
attributes (Frame metadata _) =
    metadata.attributes


{-| -}
unit : Frame units coordinates -> Unit
unit (Frame metadata _) =
    metadata.unit



-- Modifiers


theMetadata : Frame units coordinates -> Metadata
theMetadata (Frame metadata _) =
    metadata


setMetadata : Metadata -> Frame units coordinates -> Frame units coordinates
setMetadata metadata (Frame _ theBody) =
    Frame metadata theBody


{-| -}
setAuthor : String -> Frame units coordinates -> Frame units coordinates
setAuthor newAuthor frame =
    let
        metadata =
            theMetadata frame
    in
    setMetadata { metadata | author = newAuthor } frame


{-| -}
setTitle : String -> Frame units coordinates -> Frame units coordinates
setTitle newTitle frame =
    let
        metadata =
            theMetadata frame
    in
    setMetadata { metadata | title = newTitle } frame


{-| -}
setDescription : String -> Frame units coordinates -> Frame units coordinates
setDescription newDescription frame =
    let
        metadata =
            theMetadata frame
    in
    setMetadata { metadata | description = newDescription } frame


{-| -}
setClasses : List Class -> Frame units coordinates -> Frame units coordinates
setClasses newClasses frame =
    let
        metadata =
            theMetadata frame
    in
    setMetadata { metadata | classes = newClasses } frame


{-| -}
setAttributes : List Attribute -> Frame units coordinates -> Frame units coordinates
setAttributes newAttributes frame =
    let
        metadata =
            theMetadata frame
    in
    setMetadata { metadata | attributes = newAttributes } frame


{-| -}
setUnit : Unit -> Frame units coordinates -> Frame units coordinates
setUnit newUnit frame =
    let
        metadata =
            theMetadata frame
    in
    setMetadata { metadata | unit = newUnit } frame



-- Vertices


{-| -}
vertices : Frame units coordinates -> List (Vertex units coordinates)
vertices (Frame _ theBody) =
    theBody.vertices
        |> Dict.values


{-| -}
vertexAdjacencies : Frame units coordinates -> Vertex units coordinates -> List (Vertex units coordinates)
vertexAdjacencies (Frame _ theBody) (Vertex vertexProperties) =
    List.filterMap (\vertex -> Dict.get vertex theBody.vertices) vertexProperties.adjacency


{-| -}
vertexFaces : Frame units coordinates -> Vertex units coordinates -> List Face
vertexFaces (Frame _ theBody) (Vertex vertexProperties) =
    List.filterMap (\face -> Dict.get face theBody.faces) vertexProperties.faces



-- Edges


{-| -}
edges : Frame units coordinates -> List Edge
edges (Frame _ theBody) =
    theBody.edges
        |> Dict.values


{-| -}
edgeStartVertex : Edge -> Frame units coordinates -> Maybe (Vertex units coordinates)
edgeStartVertex (Edge { startVertex }) (Frame _ theBody) =
    theBody.vertices
        |> Dict.get startVertex


{-| -}
edgeEndVertex : Edge -> Frame units coordinates -> Maybe (Vertex units coordinates)
edgeEndVertex (Edge { endVertex }) (Frame _ theBody) =
    Dict.get endVertex theBody.vertices


{-| -}
edgeVertices : Edge -> Frame units coordinates -> List (Vertex units coordinates)
edgeVertices edge frame =
    List.filterMap identity [ edgeStartVertex edge frame, edgeEndVertex edge frame ]


{-| -}
edgeFaces : Frame units coordinates -> Edge -> List Face
edgeFaces (Frame _ theBody) (Edge edgeProperties) =
    List.filterMap (\face -> Dict.get face theBody.faces) edgeProperties.faces



-- Faces


{-| -}
faces : Frame units coordinates -> List Face
faces (Frame _ theBody) =
    theBody.faces
        |> Dict.values


{-| -}
faceVertices : Frame units coordinates -> Face -> List (Vertex units coordinates)
faceVertices (Frame _ theBody) (Face faceProperties) =
    List.filterMap (\vertex -> Dict.get vertex theBody.vertices) faceProperties.vertices


{-| -}
faceEdges : Frame units coordinates -> Face -> List Edge
faceEdges (Frame _ theBody) (Face faceProperties) =
    List.filterMap (\edge -> Dict.get edge theBody.edges) faceProperties.edges



-- Json
-- Encode


{-| -}
encode : Frame units coordinates -> Value
encode frame =
    Encode.object <| encodePartial frame


{-| -}
encodePartial : Frame units coordinates -> List ( String, Value )
encodePartial frame =
    encodeMetadata frame ++ encodeBody frame


encodeMetadata : Frame units coordinates -> List ( String, Value )
encodeMetadata (Frame metadata _) =
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
    [ ( "frame_author", Encode.string metadata.author )
    , ( "frame_title", Encode.string metadata.title )
    , ( "frame_description", Encode.string metadata.description )
    , ( "frame_classes", Encode.list encodeClass metadata.classes )
    , ( "frame_attributes", Encode.list encodeAttribute metadata.attributes )
    , ( "frame_unit", Encode.string (Type.unitToString metadata.unit) )
    ]


{-| -}
encodeBody : Frame units coordinates -> List ( String, Value )
encodeBody (Frame _ body) =
    encodeVertices (Dict.values body.vertices)
        ++ encodeEdges (Dict.values body.edges)
        ++ encodeFaces (Dict.values body.faces)


encodeIds : List (List Int) -> Value
encodeIds =
    Encode.list <| Encode.list Encode.int


encodeVertices : List (Vertex units coordinates) -> List ( String, Value )
encodeVertices theVertices =
    let
        map property =
            List.map (\(Vertex properties) -> property properties)

        encodePoints =
            Encode.list
                (\vertex ->
                    Encode.list Encode.float
                        [ Quantity.unwrap <| Point2d.xCoordinate vertex
                        , Quantity.unwrap <| Point2d.yCoordinate vertex
                        ]
                )
    in
    [ ( "vertices_coords", encodePoints (map .coordinate theVertices) )
    , ( "vertices_vertices", encodeIds (map .adjacency theVertices) )
    , ( "vertices_faces", encodeIds (map .faces theVertices) )
    ]


encodeEdges : List Edge -> List ( String, Value )
encodeEdges theEdges =
    let
        map property =
            List.map (\(Edge properties) -> property properties)

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

        encodeEdgeVertices (Edge edge) =
            Encode.list Encode.int [ edge.startVertex, edge.endVertex ]
    in
    [ ( "edges_vertices", Encode.list encodeEdgeVertices theEdges )
    , ( "edges_faces", encodeIds (map .faces theEdges) )
    , ( "edges_assignment", Encode.list encodeEdgeType (map .edgeType theEdges) )
    , ( "edges_foldAngle", Encode.list encodeAngle (map .foldAngle theEdges) )
    ]


encodeFaces : List Face -> List ( String, Value )
encodeFaces theFaces =
    let
        map property =
            List.map (\(Face properties) -> property properties)
    in
    [ ( "faces_vertices", encodeIds (map .vertices theFaces) )
    , ( "faces_edges", encodeIds (map .edges theFaces) )
    ]



-- Decoder


{-| This is a decoder for a frame object. This decoder is used for decoding a
full frame object. This should be used for decoding every frame except the
key frame. The key frame is the frame that is stored in the top level object.
-}
decoder : Decoder (Frame units coordinates)
decoder =
    Decode.map2 Frame
        decoderMetadata
        decoderBody


decoderMetadata : Decoder Metadata
decoderMetadata =
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
    Decode.map6 Metadata
        (Decode.maybeString "frame_author")
        (Decode.maybeString "frame_title")
        (Decode.maybeString "frame_description")
        (Decode.maybeList "frame_classes" decodeClass)
        (Decode.maybeList "frame_attributes" decodeAttribute)
        (Decode.maybeWithDefault "frame_unit" decodeUnit Unit)


decoderBody : Decoder (Body units coordinates)
decoderBody =
    let
        decodedVertices verticesRecord =
            let
                fillEmpty =
                    List.fillEmpty (List.length verticesRecord.vertices) []
            in
            List.indexedMap
                (\index ( point, adjacencies, theFaces ) ->
                    Vertex
                        { coordinate = point
                        , adjacency = adjacencies
                        , faces = theFaces
                        , id = index
                        }
                )
                (List.Extra.zip3
                    verticesRecord.vertices
                    (fillEmpty verticesRecord.adjacency)
                    (fillEmpty verticesRecord.faces)
                )

        decodedEdges edgesRecord =
            let
                numEdges =
                    List.length edgesRecord.vertices
            in
            List.indexedMap
                (\index { endpoints, theFaces, edgeType, foldAngle } ->
                    Edge
                        { startVertex = Tuple.first endpoints
                        , endVertex = Tuple.second endpoints
                        , faces = theFaces
                        , edgeType = edgeType
                        , foldAngle = foldAngle
                        , id = index
                        }
                )
                (List.map4
                    (\endpoints theFaces assignment foldAngle ->
                        { endpoints = endpoints
                        , theFaces = theFaces
                        , edgeType = assignment
                        , foldAngle = foldAngle
                        }
                    )
                    edgesRecord.vertices
                    (List.fillEmpty numEdges [] edgesRecord.faces)
                    (List.fillEmpty numEdges Mountain edgesRecord.assignment)
                    (List.fillEmpty numEdges (Angle.radians 0) edgesRecord.foldAngle)
                )

        decodedFaces facesRecord =
            List.indexedMap
                (\index ( theVertices, theEdges ) ->
                    Face
                        { vertices = theVertices
                        , edges = theEdges
                        , id = index
                        }
                )
                (List.Extra.zip
                    facesRecord.vertices
                    facesRecord.edges
                )

        toDictionary =
            List.indexedMap Tuple.pair >> Dict.fromList
    in
    Decode.map3
        (\verticesRecord edgesRecord facesRecord ->
            Body
                (toDictionary <| decodedVertices verticesRecord)
                (toDictionary <| decodedEdges edgesRecord)
                (toDictionary <| decodedFaces facesRecord)
        )
        decoderVertices
        decoderEdges
        decoderFaces


decodeIds : String -> Decoder (List (List Int))
decodeIds name =
    Decode.maybeList name <| Decode.list Decode.int


type alias Vertices units coordinates =
    { vertices : List (Point2d units coordinates)
    , adjacency : List (List Int)
    , faces : List (List Int)
    }


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
        (decodeIds "vertices_vertices")
        (decodeIds "vertices_faces")


type alias Edges =
    { vertices : List ( Int, Int )
    , faces : List (List Int)
    , assignment : List EdgeType
    , foldAngle : List Angle
    }


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

        decoderEdgeVertices =
            Decode.list Decode.int
                |> Decode.andThen
                    (\list ->
                        case list of
                            a :: b :: [] ->
                                Decode.succeed ( a, b )

                            _ ->
                                Decode.fail <| "Edge vertices must be of length 2"
                    )
                |> Decode.list
    in
    Decode.map4 Edges
        (Decode.field "edges_vertices" decoderEdgeVertices)
        (decodeIds "edges_faces")
        (Decode.field "edges_assignment" <| Decode.list decoderEdgeType)
        (Decode.maybeList "edges_foldAngle" decoderAngle)


type alias Faces =
    { vertices : List (List Int)
    , edges : List (List Int)
    }


decoderFaces : Decoder Faces
decoderFaces =
    Decode.map2 Faces
        (decodeIds "faces_vertices")
        (decodeIds "faces_edges")
