module Page.GameDetails exposing (..)

import Browser
import Chat
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Firebase
import Html
import Page.PokerTable
import Poker as Poker
import Styles exposing (..)



viewGameDetailsPage : Element msg
viewGameDetailsPage =
    column
        [ height fill
        , width fill
        , Background.color darkgray
        , Font.color white
        , Font.family
            [ Font.typeface "Lato"
            , Font.typeface "Helvetica Neue"
            , Font.typeface "Helvetica"
            , Font.typeface "Arial"
            , Font.sansSerif
            ]
        ]
        [ row
            [ width fill
            , paddingXY 15 13
            , Background.color primary
            ]
            [ el [ alignLeft ] hamburger
            , el [ alignBottom, centerX, paddingEach { top = 2, bottom = 2, left = 0, right = 0 } ] (logo "NigelGames")
            , el [ alignRight, width <| px 59 ] none
            ]
        , row
            [ Background.color black
            , width fill
            , padding 13
            , Font.size 14
            , Border.shadow { offset = ( 0, 5 ), size = 1, blur = 10, color = black3 }
            ]
            [ el [ centerX ] (text "Looking for more players? Join the Discord Server!") ]
        , row
            [ width fill
            , height fill
            ]
            [ column
                [ centerX
                , height fill
                , width <| px 480
                , padding 7
                , spacing 0

                --            , Element.explain Debug.todo
                ]
                [ column [ width fill ]
                    [ h1hr
                    , column [ width fill, spacing 11 ]
                        [ h1 "Poker"
                        , row [ spacing 6, centerX, padding 3 ] [ badge "deceit", badge "deduction" ]
                        ]
                    , h1hr
                    ]
                , paragraph [ Font.center, spacing 8, padding 4 ]
                    [ text "Poker is a game of risk, deduction, and luck for 2–8 players. Your goal is to win all the chips."
                    ]
                ]
            ]
        , row [ width fill, Border.shadow { offset = ( 0, 0 ), blur = 40, size = 20, color = black75 } ]
            -- button row
            [ column [ padding 10, width fill, spacing 10, Background.color (Element.rgb255 34 34 34) ]
                [ button "New Game"
                , button "Join Game"
                ]
            ]
        ]


h1hr =
    el [ width fill, paddingXY 0 21 ] (el [ width fill, Background.color barcol, height <| px 1 ] Element.none)


h1 label =
    el [ centerX, Font.size 39, padding 2 ] (text label)


badge label =
    el [ Border.rounded 4, Background.color barcol, paddingXY 13 3, Font.size 15, Font.bold ] (text label)


centertext label =
    el [ centerX ] (text label)


hr color =
    row [ width fill, height <| px 2, Background.color color ] []


logo label =
    el [ Font.color white, Font.size 26, Font.family [ Font.typeface "Baloo", Font.sansSerif ] ] (text label)


hamburger =
    Input.button
        [ padding 5
        , width <| px 44
        , height <| px 34
        , Background.color primary
        , Border.color bluegray
        , Border.width 1
        , Border.rounded 4
        ]
        { label = hamburgerIcon
        , onPress = Nothing
        }


hamburgerIcon =
    column [ width <| px 22, spacing 4, paddingXY 0 0, centerX, centerY ] [ hr white, hr white, hr white ]


button label =
    Input.button
        [ centerY
        , centerX
        , width fill
        , Font.size 36
        , Font.family
            [ Font.typeface "Lato"
            , Font.typeface "Helvetica Neue"
            , Font.typeface "Helvetica"
            , Font.typeface "Arial"
            , Font.sansSerif
            ]
        , paddingEach { top = 21, bottom = 23, left = 0, right = 0 }
        , Background.color primary
        , Element.mouseOver [ Background.color hover ]
        , Border.rounded 4
        ]
        { label = el [ centerX, Font.color (Element.rgb255 255 255 255) ] (text label)
        , onPress = Nothing
        }

