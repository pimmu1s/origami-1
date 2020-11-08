module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Fold.Edge as Edge
import Fold.File exposing (Class(..))
import Fold.Frame as Frame exposing (Attribute(..), Class(..))
import Fold.Types as Types exposing (EdgeType(..))
import Fold.Vertex as Vertex
import Html exposing (Html)
import Json.Decode as Decode
import Pixels exposing (Pixels)
import Point2d
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


type alias Model =
    { foldFile : FoldFile
    }


type TopLeftCoordinates
    = TopLeftCoordinates


type FoldFile
    = NoFoldFile
    | FoldFileError Decode.Error
    | FoldFile (Fold.File.File Pixels TopLeftCoordinates)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { foldFile = NoFoldFile
      }
    , Cmd.none
    )



-- Update


type Msg
    = FoldFileRequest
    | FoldFileLoaded File
    | FoldFileContents String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FoldFileRequest ->
            ( model, Select.file [ "application/fold" ] FoldFileLoaded )

        FoldFileLoaded foldFile ->
            ( model, Task.perform FoldFileContents <| File.toString foldFile )

        FoldFileContents foldContents ->
            case Decode.decodeString Fold.File.decoder foldContents of
                Ok foldFile ->
                    ( { model | foldFile = FoldFile foldFile }
                    , Cmd.none
                    )

                Err foldErrors ->
                    ( { model | foldFile = FoldFileError foldErrors }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    let
        upload =
            Input.button []
                { onPress = Just FoldFileRequest
                , label = text "Upload FOLD File"
                }

        foldText =
            case model.foldFile of
                NoFoldFile ->
                    none

                FoldFile foldFile ->
                    viewFoldFile foldFile

                FoldFileError error ->
                    text <| Decode.errorToString error
    in
    Element.layout [ padding 50 ] <|
        column []
            [ upload
            , foldText
            ]


viewFoldFile : Fold.File.File units coordinates -> Element msg
viewFoldFile foldFile =
    let
        fileAttributes =
            mapAttributes <|
                [ ( "Spec", String.fromInt <| Fold.File.spec foldFile )
                , ( "Creator", Fold.File.creator foldFile )
                , ( "Author", Fold.File.author foldFile )
                , ( "Title", Fold.File.title foldFile )
                , ( "Description", Fold.File.description foldFile )
                , ( "Attributes", mapList Fold.File.classes fileClassToString foldFile )
                ]

        frameAttributes frame =
            mapAttributes <|
                [ ( "Author", Frame.author frame )
                , ( "Title", Frame.title frame )
                , ( "Description", Frame.description frame )
                , ( "Classes", mapList Frame.classes frameClassToString frame )
                , ( "Attributes", mapList Frame.attributes frameAttributeToString frame )
                , ( "Unit", Types.unitToString <| Frame.unit frame )
                , ( "Vertices", mapList Frame.vertices vertexToString frame )
                , ( "Edges", mapList Frame.edges edgeTypeToString frame )
                ]

        vertexToString vert =
            Vertex.coordinate vert
                |> Point2d.unwrap
                |> (\{ x, y } ->
                        "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"
                   )

        edgeTypeToString edge =
            Edge.edgeType edge
                |> (\edgeType ->
                        case edgeType of
                            Boundary ->
                                "Boundary"

                            Mountain ->
                                "Mountain"

                            Valley ->
                                "Valley"

                            Flat ->
                                "Flat"

                            Unassigned ->
                                "Unassigned"
                   )

        fileClassToString class =
            case class of
                SingleModel ->
                    "Single Model"

                MultiModel ->
                    "Multi Model"

                Animation ->
                    "Animation"

                Diagrams ->
                    "Diagrams"

        frameClassToString class =
            case class of
                CreasePattern ->
                    "Crease Pattern"

                FoldedForm ->
                    "Folded Form"

                Graph ->
                    "Graph"

                Linkage ->
                    "Linkage"

        frameAttributeToString attribute =
            case attribute of
                Dimension2d ->
                    "2D"

                Dimension3d ->
                    "3D"

                Abstract ->
                    "Abstract"

                Manifold ->
                    "Manifold"

                NonManifold ->
                    "Non-Manifold"

                Orientable ->
                    "Orientable"

                SelfTouching ->
                    "Non-Orientable"

                NonSelfTouching ->
                    "Self Touching"

                SelfIntersecting ->
                    "Non Self Touching"

                NonSelfInteresting ->
                    "Non Self Intersecting"

        mapList accessor toString file =
            accessor file
                |> List.map toString
                |> String.join ", "

        mapAttributes =
            List.map
                (\( name, value ) ->
                    paragraph [ spacing 6 ]
                        [ el [ Font.bold ] <| text (name ++ ": ")
                        , text value
                        ]
                )

        fileColumn =
            column [ padding 10, spacing 6 ]
    in
    fileColumn
        (List.map fileColumn
            ([ fileAttributes ]
                ++ List.map frameAttributes (Fold.File.allFrames foldFile)
            )
        )
