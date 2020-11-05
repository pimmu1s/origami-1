module Test.Fold.File exposing (..)

import Expect exposing (Expectation)
import Fold.File as File
import Json.Decode as Decode
import Test exposing (..)


encodeAndDecode : Test
encodeAndDecode =
    let
        file =
            File.with
                { spec = "1.0"
                , creator = "Elm Application"
                , author = "Author"
                , title = "Test"
                , description = "Test file"
                , classes = [ File.SingleModel ]
                , frames = []
                }
    in
    test "Encoding and decoding fold file properties" <|
        \_ ->
            File.encode file
                |> Decode.decodeValue File.decoder
                |> Expect.equal (Ok file)
