module Fold.File exposing
    ( File, Class(..)
    , empty, with
    , spec, creator, author, title, description, classes
    , setSpec, setCreator, setAuthor, setTitle, setDescription, setClasses
    , encode, decoder
    )

{-|


# Types

@docs File, Class


# Builders

@docs empty, with


# Metadata


## Accessors

@docs spec, creator, author, title, description, classes


## Modifiers

@docs setSpec, setCreator, setAuthor, setTitle, setDescription, setClasses


# Json

@docs encode, decoder

-}

import Fold.Frame as Frame exposing (Frame)
import Fold.Types exposing (Unit(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util.Decode as Decode


{-| -}
type File units coordinates
    = File Metadata (Frame units coordinates) (List (Frame units coordinates))


{-| -}
type alias Metadata =
    { spec : Int
    , creator : String
    , author : String
    , title : String
    , description : String
    , classes : List Class
    }


{-| -}
type Class
    = SingleModel
    | MultiModel
    | Animation
    | Diagrams



-- Builders


{-| -}
empty : File units coordinates
empty =
    with
        { spec = 1
        , creator = "Elm Library"
        , author = ""
        , title = ""
        , description = ""
        , classes = []
        }
        Frame.empty


{-| -}
with :
    { spec : Int
    , creator : String
    , author : String
    , title : String
    , description : String
    , classes : List Class
    }
    -> Frame units coordinates
    -> File units coordinates
with theMetadata keyFrame =
    File theMetadata keyFrame []



-- Metadata


metadata : File units coordinates -> Metadata
metadata (File theMetadata _ _) =
    theMetadata


{-| -}
spec : File units coordinates -> Int
spec (File theMetadata _ _) =
    theMetadata.spec


{-| -}
creator : File units coordinates -> String
creator (File theMetadata _ _) =
    theMetadata.creator


{-| -}
author : File units coordinates -> String
author (File theMetadata _ _) =
    theMetadata.author


{-| -}
title : File units coordinates -> String
title (File theMetadata _ _) =
    theMetadata.title


{-| -}
description : File units coordinates -> String
description (File theMetadata _ _) =
    theMetadata.description


{-| -}
classes : File units coordinates -> List Class
classes (File theMetadata _ _) =
    theMetadata.classes



-- Modifiers


setMetadata : Metadata -> File units coordinates -> File units coordinates
setMetadata newMetadata (File _ keyFrame otherFrames) =
    File newMetadata keyFrame otherFrames


{-| -}
setSpec : Int -> File units coordinates -> File units coordinates
setSpec newSpec file =
    let
        theMetadata =
            metadata file
    in
    setMetadata { theMetadata | spec = newSpec } file


{-| -}
setCreator : String -> File units coordinates -> File units coordinates
setCreator newCreator file =
    let
        theMetadata =
            metadata file
    in
    setMetadata { theMetadata | creator = newCreator } file


{-| -}
setAuthor : String -> File units coordinates -> File units coordinates
setAuthor newAuthor file =
    let
        theMetadata =
            metadata file
    in
    setMetadata { theMetadata | author = newAuthor } file


{-| -}
setTitle : String -> File units coordinates -> File units coordinates
setTitle newTitle file =
    let
        theMetadata =
            metadata file
    in
    setMetadata { theMetadata | title = newTitle } file


{-| -}
setDescription : String -> File units coordinates -> File units coordinates
setDescription newDescription file =
    let
        theMetadata =
            metadata file
    in
    setMetadata { theMetadata | description = newDescription } file


{-| -}
setClasses : List Class -> File units coordinates -> File units coordinates
setClasses newClasses file =
    let
        theMetadata =
            metadata file
    in
    setMetadata { theMetadata | classes = newClasses } file



-- Json


{-| -}
encode : File units coordinates -> Value
encode (File properties theKeyFrame theOtherFrames) =
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
    Encode.object <|
        [ ( "file_spec", Encode.int properties.spec )
        , ( "file_creator", Encode.string properties.creator )
        , ( "file_author", Encode.string properties.author )
        , ( "file_title", Encode.string properties.title )
        , ( "file_description", Encode.string properties.description )
        , ( "file_classes", Encode.list encodeClass properties.classes )
        , ( "file_frames", Encode.list Frame.encode theOtherFrames )
        ]
            ++ Frame.encodeBody theKeyFrame


{-| -}
decoder : Decoder (File units coordinates)
decoder =
    Decode.map3
        (\theMetadata keyFrameBody frames ->
            let
                keyFrame =
                    Frame.with
                        (Frame.header
                            { author = theMetadata.author
                            , title = theMetadata.title
                            , description = theMetadata.description
                            , classes = []
                            , attributes = []
                            , unit = Unit
                            }
                        )
                        keyFrameBody
            in
            File theMetadata keyFrame frames
        )
        decoderMetadata
        Frame.decoderBody
        (Decode.maybeList "file_frames" Frame.decoder)


{-| -}
decoderMetadata : Decoder Metadata
decoderMetadata =
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
    in
    Decode.map6 Metadata
        (Decode.maybeInt "file_spec" 1)
        (Decode.maybeString "file_creator")
        (Decode.maybeString "file_author")
        (Decode.maybeString "file_title")
        (Decode.maybeString "file_description")
        (Decode.maybeList "file_classes" decodeClasses)
