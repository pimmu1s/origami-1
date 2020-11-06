module Test.Fold.Vertex exposing (..)

import Expect
import Fold.Vertex as Vertex
import Json.Decode as Decode
import Point2d
import Quantity
import Test exposing (Test, test)


encodeAndDecode : Test
encodeAndDecode =
    let
        vertex =
            Vertex.at <|
                Point2d.xy
                    (Quantity.float 1)
                    (Quantity.float 1)
    in
    test "Encoding and decoding fold frame properties" <|
        \_ ->
            Vertex.encode vertex
                |> Decode.decodeValue Vertex.decoder
                |> Expect.equal (Ok vertex)
