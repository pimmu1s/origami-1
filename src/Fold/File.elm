module Fold.File exposing
    ( File, Class(..)
    , empty, with
    , spec, creator, author, title, description, classes, frames
    , setSpec, setCreator, setAuthor, setTitle, setDescription, setClasses, setFrames
    , encode, decoder
    )

{-|


# Types

@docs File, Class


# Builders

@docs empty, with


# Accessors

@docs spec, creator, author, title, description, classes, frames


# Modifiers

@docs setSpec, setCreator, setAuthor, setTitle, setDescription, setClasses, setFrames


# Json

@docs encode, decoder

-}

import Fold.Frame as Frame exposing (Frame)
import Json.Decode as Decode
import Json.Encode as Encode


{-| -}
type File
    = File Properties


{-| -}
type alias Properties =
    { spec : String
    , creator : String
    , author : String
    , title : String
    , description : String
    , classes : List Class
    , frames : List Frame
    }


{-| -}
type Class
    = SingleModel
    | MultiModel
    | Animation
    | Diagrams



-- Builders


{-| -}
empty : File
empty =
    with
        { spec = "1.1"
        , creator = "Elm Library"
        , author = ""
        , title = ""
        , description = ""
        , classes = []
        , frames = []
        }


{-| -}
with :
    { spec : String
    , creator : String
    , author : String
    , title : String
    , description : String
    , classes : List Class
    , frames : List Frame
    }
    -> File
with =
    File



-- Accessors


{-| -}
spec : File -> String
spec (File properties) =
    properties.spec


{-| -}
creator : File -> String
creator (File properties) =
    properties.creator


{-| -}
author : File -> String
author (File properties) =
    properties.author


{-| -}
title : File -> String
title (File properties) =
    properties.title


{-| -}
description : File -> String
description (File properties) =
    properties.description


{-| -}
classes : File -> List Class
classes (File properties) =
    properties.classes


{-| -}
frames : File -> List Frame
frames (File properties) =
    properties.frames



-- Modifiers


{-| -}
setSpec : String -> File -> File
setSpec newSpec (File properties) =
    File { properties | spec = newSpec }


{-| -}
setCreator : String -> File -> File
setCreator newCreator (File properties) =
    File { properties | creator = newCreator }


{-| -}
setAuthor : String -> File -> File
setAuthor newAuthor (File properties) =
    File { properties | author = newAuthor }


{-| -}
setTitle : String -> File -> File
setTitle newTitle (File properties) =
    File { properties | title = newTitle }


{-| -}
setDescription : String -> File -> File
setDescription newDescription (File properties) =
    File { properties | description = newDescription }


{-| -}
setClasses : List Class -> File -> File
setClasses newClasses (File properties) =
    File { properties | classes = newClasses }


{-| -}
setFrames : List Frame -> File -> File
setFrames newFrames (File properties) =
    File { properties | frames = newFrames }



-- Json


{-| -}
encode : File -> Encode.Value
encode (File properties) =
    let
        encodeClass =
            \class ->
                Encode.string <|
                    case class of
                        SingleModel ->
                            "singleModel"

                        MultiModel ->
                            "multiModel"

                        Animation ->
                            "animation"

                        Diagrams ->
                            "diagrams"
    in
    Encode.object
        [ ( "file_spec", Encode.string properties.spec )
        , ( "file_creator", Encode.string properties.creator )
        , ( "file_author", Encode.string properties.author )
        , ( "file_title", Encode.string properties.title )
        , ( "file_description", Encode.string properties.description )
        , ( "file_classes", Encode.list encodeClass properties.classes )
        , ( "file_frames", Encode.list Frame.encode properties.frames )
        ]


{-| -}
decoder : Decode.Decoder File
decoder =
    let
        decodeClasses =
            Decode.string
                |> Decode.andThen
                    (\string ->
                        case string of
                            "singleModel" ->
                                Decode.succeed SingleModel

                            "multiModel" ->
                                Decode.succeed MultiModel

                            "animation" ->
                                Decode.succeed Animation

                            "diagrams" ->
                                Decode.succeed Diagrams

                            _ ->
                                Decode.fail <| "\"" ++ string ++ "\" is not a valid file class."
                    )
                |> Decode.list
    in
    Decode.map7 Properties
        (Decode.field "file_spec" Decode.string)
        (Decode.field "file_creator" Decode.string)
        (Decode.field "file_author" Decode.string)
        (Decode.field "file_title" Decode.string)
        (Decode.field "file_description" Decode.string)
        (Decode.field "file_classes" decodeClasses)
        (Decode.field "file_frames" <| Decode.list Frame.decoder)
        |> Decode.map File
