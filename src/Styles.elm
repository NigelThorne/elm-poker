module Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode


buttonStyle =
    [ height fill
    , width <| fillPortion 1
    , Background.color <| rgb255 92 99 118
    , Font.color <| rgb255 255 255 255
    , spacing 8
    , padding 8
    ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )

-- onMouseOver msg =
--   Dom.Element.addAttribute
--     ( "mouseover"
--       |> Dom.Event.action msg
--     )

hover =
    Element.rgb255 0x28 0x41 0x5B


barcol =
    Element.rgb255 0x46 0x45 0x45


black =
    Element.rgb255 0 0 0


black75 =
    Element.rgba255 0 0 0 0.75


black3 =
    Element.rgba255 0 0 0 0.3


white =
    Element.rgb255 255 255 255


bluegray =
    Element.rgb255 40 65 91


primary =
    Element.rgb255 55 90 127


darkgray =
    Element.rgb255 34 34 34

