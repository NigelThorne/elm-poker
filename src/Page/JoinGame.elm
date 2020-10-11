module Page.JoinGame exposing (..)

import Browser.Navigation
import Element exposing (Element)
import Element exposing (..)
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Styles exposing (..)


type alias State =
    { gameName : String }


type Msg
    = NameChanged String
    | JoinPressed


update : State -> Msg -> ( State, Cmd msg )
update state msg =
    case msg of
        NameChanged newName ->
            ( { state | gameName = newName }, Cmd.none )

        JoinPressed ->
            ( state, Browser.Navigation.load ("/game/" ++ state.gameName) )


viewLobby : State -> Element Msg
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
                , onEnter JoinPressed
                ]
                { label = Input.labelAbove [ centerX ] (text "Enter the room code:")
                , onChange = NameChanged
                , placeholder = Nothing -- Just (Input.placeholder [] (text ""))
                , text = model.gameName
                }
            , el [ Border.width 3 ]
                (Input.button
                    [ centerY, Font.size 50, padding 20, moveUp 5 ]
                    { label = text ">"
                    , onPress = Just JoinPressed
                    }
                )
            ]
        ]
