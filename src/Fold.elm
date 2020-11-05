module Fold exposing (..)

{-| Supporting the FOLD file format by Eric Demaine.
<https://github.com/edemaine/fold/blob/master/doc/spec.md>
-}

import Json.Decode as Decode
import Json.Encode as Encode


type Fold
    = Fold
        { spec : String
        , creator : String
        , author : String
        , title : String
        , description : String
        , classes : List Classes
        , frames : String
        }


type Classes
    = SingleModel
    | MultiModel
    | Animation
    | Diagrams



-- Builder


{-| Create an empty fold object.
-}
empty : Fold
empty =
    Fold
        { spec = "1.1"
        , creator = "Elm Library"
        , author = ""
        , title = ""
        , description = ""
        , classes = []
        , frames = ""
        }
