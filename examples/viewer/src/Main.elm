module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Fold.File
import Html exposing (Html)
import Json.Decode as Decode
import Pixels exposing (Pixels)
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

                FoldFile _ ->
                    text "File Loaded"

                FoldFileError error ->
                    text <| Decode.errorToString error
    in
    Element.layout [] <|
        column []
            [ upload
            , foldText
            ]
