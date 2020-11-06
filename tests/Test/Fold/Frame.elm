module Test.Fold.Frame exposing (..)

import Expect
import Fold.Frame as Frame exposing (..)
import Fold.Types as Types
import Json.Decode as Decode
import Test exposing (Test, test)


encodeAndDecode : Test
encodeAndDecode =
    let
        frame =
            empty
                |> setAuthor "The Author"
                |> setTitle "First Frame"
                |> setDescription "The first frame of the crease pattern"
                |> setClasses [ Frame.CreasePattern ]
                |> setAttributes [ Frame.Dimension2d ]
                |> setUnit Types.Unit
    in
    test "Encoding and decoding fold frame properties" <|
        \_ ->
            Frame.encode frame
                |> Decode.decodeValue Frame.decoder
                |> Expect.equal (Ok frame)
