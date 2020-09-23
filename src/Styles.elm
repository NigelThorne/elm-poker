module Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input


buttonStyle =
    [ height fill
    , width <| fillPortion 1
    , Background.color <| rgb255 92 99 118
    , Font.color <| rgb255 255 255 255
    , spacing 8
    , padding 8
    ]
