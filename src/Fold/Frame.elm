module Fold.Frame exposing
    ( Frame, Class(..), Attribute(..)
    , empty, with
    , author, title, description, classes, attributes, unit
    , setAuthor, setTitle, setDescription, setClasses, setAttributes, setUnit
    , encode, decoder
    )

{-|


# Types

@docs Frame, Class, Attribute


# Builder

@docs empty, with


# Accessors

@docs author, title, description, classes, attributes, unit


# Modifiers

@docs setAuthor, setTitle, setDescription, setClasses, setAttributes, setUnit


# Json

@docs encode, decoder

-}

import Fold.Types as Type exposing (Unit(..))
import Json.Decode as Decode
import Json.Encode as Encode


{-| -}
type Frame
    = Frame Properties


{-| -}
type alias Properties =
    { author : String
    , title : String
    , description : String
    , classes : List Class
    , attributes : List Attribute
    , unit : Unit
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
empty : Frame
empty =
    with
        { author = ""
        , title = ""
        , description = ""
        , classes = []
        , attributes = []
        , unit = Unit
        }


{-| -}
with :
    { author : String
    , title : String
    , description : String
    , classes : List Class
    , attributes : List Attribute
    , unit : Unit
    }
    -> Frame
with =
    Frame



-- Accessors


{-| -}
author : Frame -> String
author (Frame properties) =
    properties.author


{-| -}
title : Frame -> String
title (Frame properties) =
    properties.title


{-| -}
description : Frame -> String
description (Frame properties) =
    properties.description


{-| -}
classes : Frame -> List Class
classes (Frame properties) =
    properties.classes


{-| -}
attributes : Frame -> List Attribute
attributes (Frame properties) =
    properties.attributes


{-| -}
unit : Frame -> Unit
unit (Frame properties) =
    properties.unit



-- Modifiers


{-| -}
setAuthor : String -> Frame -> Frame
setAuthor newAuthor (Frame properties) =
    Frame { properties | author = newAuthor }


{-| -}
setTitle : String -> Frame -> Frame
setTitle newTitle (Frame properties) =
    Frame { properties | title = newTitle }


{-| -}
setDescription : String -> Frame -> Frame
setDescription newDescription (Frame properties) =
    Frame { properties | description = newDescription }


{-| -}
setClasses : List Class -> Frame -> Frame
setClasses newClasses (Frame properties) =
    Frame { properties | classes = newClasses }


{-| -}
setAttributes : List Attribute -> Frame -> Frame
setAttributes newAttributes (Frame properties) =
    Frame { properties | attributes = newAttributes }


{-| -}
setUnit : Unit -> Frame -> Frame
setUnit newUnit (Frame properties) =
    Frame { properties | unit = newUnit }



-- Json


{-| -}
encode : Frame -> Encode.Value
encode (Frame properties) =
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
    Encode.object
        [ ( "frame_author", Encode.string properties.author )
        , ( "frame_title", Encode.string properties.title )
        , ( "frame_description", Encode.string properties.description )
        , ( "frame_classes", Encode.list encodeClass properties.classes )
        , ( "frame_attributes", Encode.list encodeAttribute properties.attributes )
        , ( "frame_unit", Encode.string (Type.unitToString properties.unit) )
        ]


{-| -}
decoder : Decode.Decoder Frame
decoder =
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
        |> Decode.map Frame
