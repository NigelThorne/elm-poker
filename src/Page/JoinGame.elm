module Page.JoinGame exposing (..)

viewLobby : Model -> Element Msg
viewLobby model =
    column
        [ height fill
        , width fill
        ]
        [ el [ centerX, centerY, Font.bold, Font.size 120 ] (text "Welcome!")

        --        , el [centerX, centerY] (text "Welcome!")
        , row [ centerX, centerY, padding 100, spacing 10, width fill ]
            [ Input.text
                [ width fill
                , spacing 10
                , centerX
                , onEnter LobbyEnterWasPressed
                ]
                { label = Input.labelAbove [ centerX ] (text "Enter the room code:")
                , onChange = NextRoomNameChanged
                , placeholder = Nothing -- Just (Input.placeholder [] (text ""))
                , text = model.nextRoomName
                }
            , el [ Border.width 3 ]
                (Input.button
                    [ centerY, Font.size 50, padding 20, moveUp 5 ]
                    { label = text ">"
                    , onPress = Just (JoinRoom model.nextRoomName)
                    }
                )
            ]
        ]