module MyRegex exposing (..)

{-| A wrapper for regex module to avoid runtime error
@docs safeRegex
-}

import Native.MyRegex
import Result
import Regex exposing (..)


{-| Safe Regex
-}
safeRegex : String -> Result String Regex
safeRegex str =
    Native.MyRegex.safeRegex
