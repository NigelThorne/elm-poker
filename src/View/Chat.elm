module View.Chat exposing (..)

import Data.Chat as Chat
import Element exposing (..)
import Browser
import Browser.Navigation as Nav
import Data.Chat as Chat
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Page.GameDetails exposing (..)
import Ports exposing (..)
import Route
import Styles exposing (..)
import Url

viewChatWindow : Chat.Model ->  Element Chat.Msg
viewChatWindow model  =
    column
        [ width <| px 300
        , spacing 10
        , centerX
        ]
        [ column [ centerX ]
            [ column
                [ spacing 10
                , Border.solid
                , Border.width 1
                , padding 10
                , width <| px 300
                , height <| px 600
                ]
                [ el [ centerX ] (text "Messages:")
                , column [ scrollbars, height fill, width fill ] <|
                    List.map
                        (\m -> paragraph [] [ text m.text ])
                        model.messages
                ]
            ]
        ,   column [ spacing 10, centerX, width <| px 300 ]
                [ Input.text
                    [ onEnter Chat.EnterWasPressed ]
                    { label = Input.labelHidden "Message to send" -- [] (el [centerX] (text "Message to send"))
                    , onChange = Chat.InputChanged
                    , placeholder = Just (Input.placeholder [] (text "message"))
                    , text = model.inputContent
                    }
                , Input.button
                    buttonStyle
                    { onPress = Just Chat.SaveMessage
                    , label = el [ centerX ] (text "Save new message")
                    }
                ]
        ]
