module Test.Fold.File exposing (..)

import Expect exposing (Expectation)
import Fold.File as File
import Fold.Frame as Frame
import Json.Decode as Decode
import Test exposing (..)


encodeAndDecode : Test
encodeAndDecode =
    let
        file =
            File.with
                { spec = 1
                , creator = "Elm Application"
                , author = "Author"
                , title = "Test"
                , description = "Test File"
                , classes = [ File.SingleModel ]
                }
                (Frame.empty
                    |> Frame.setAuthor "Author"
                    |> Frame.setTitle "Test"
                    |> Frame.setDescription "Test File"
                )
    in
    test "Encoding and decoding fold file properties" <|
        \_ ->
            File.encode file
                |> Decode.decodeValue File.decoder
                |> Expect.equal (Ok file)
