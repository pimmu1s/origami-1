module Fold exposing (Fold)

{-| Supporting the FOLD file format by Eric Demaine.
<https://github.com/edemaine/fold/blob/master/doc/spec.md>


# Types

@docs Fold

-}

import Fold.File exposing (File)


{-| -}
type Fold
    = Fold File
