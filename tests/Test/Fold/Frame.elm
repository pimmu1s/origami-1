module Test.Fold.Frame exposing (..)

import Expect
import Fold.Frame as Frame
import Fold.Types as Types
import Json.Decode as Decode
import Test exposing (Test, test)


encodeAndDecode : Test
encodeAndDecode =
    let
        frame =
            Frame.with
                { author = "The Author"
                , title = "First Frame"
                , description = "The first frame of the crease pattern"
                , classes = [ Frame.CreasePattern ]
                , attributes = [ Frame.Dimension2d ]
                , unit = Types.Unit
                }
    in
    test "Encoding and decoding fold frame properties" <|
        \_ ->
            Frame.encode frame
                |> Decode.decodeValue Frame.decoder
                |> Expect.equal (Ok frame)
