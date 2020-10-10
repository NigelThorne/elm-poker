module Page.PokerRoom exposing (..)


viewInGame : GameState -> Element Msg
viewInGame model =
    row [ height fill, width fill ]
        [ controlPanel model
        , playingArea model
        ]


controlPanel : Model -> Element Msg
controlPanel model =
    column
        [ spacing 20
        , padding 20
        , height fill
        ]
        [ el [ centerX ] (viewUserControls model)
        , el [ centerX ] (Element.map ChatMsg (viewChatWindow model.chat model.firebase))
        , el [ centerX, alignBottom, Border.width 1, padding 10, Border.rounded 8 ] (Input.button [] { onPress = Just LeaveRoom, label = text "Leave Game" })
        ]

