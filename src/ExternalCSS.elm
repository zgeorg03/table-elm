module ExternalCSS exposing (stylesheet)

{-|
  Used only for testing with reactor
-
-}

import Html exposing (..)
import Html.Attributes exposing (..)


stylesheet : Html a
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]

        children =
            []
    in
        node tag attrs children
