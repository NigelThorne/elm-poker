module Page.PokerTable exposing (..)

import Data.Poker exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Styles exposing (..)



{-



   VVVVVVVV           VVVVVVVV iiii
   V::::::V           V::::::Vi::::i
   V::::::V           V::::::V iiii
   V::::::V           V::::::V
    V:::::V           V:::::Viiiiiii     eeeeeeeeeeee  wwwwwww           wwwww           wwwwwww
     V:::::V         V:::::V i:::::i   ee::::::::::::ee w:::::w         w:::::w         w:::::w
      V:::::V       V:::::V   i::::i  e::::::eeeee:::::eew:::::w       w:::::::w       w:::::w
       V:::::V     V:::::V    i::::i e::::::e     e:::::e w:::::w     w:::::::::w     w:::::w
        V:::::V   V:::::V     i::::i e:::::::eeeee::::::e  w:::::w   w:::::w:::::w   w:::::w
         V:::::V V:::::V      i::::i e:::::::::::::::::e    w:::::w w:::::w w:::::w w:::::w
          V:::::V:::::V       i::::i e::::::eeeeeeeeeee      w:::::w:::::w   w:::::w:::::w
           V:::::::::V        i::::i e:::::::e                w:::::::::w     w:::::::::w
            V:::::::V        i::::::ie::::::::e                w:::::::w       w:::::::w
             V:::::V         i::::::i e::::::::eeeeeeee         w:::::w         w:::::w
              V:::V          i::::::i  ee:::::::::::::e          w:::w           w:::w
               VVV           iiiiiiii    eeeeeeeeeeeeee           www             www








-}


tablecolor : Color
tablecolor =
    rgb255 0 160 40


viewTable : Game -> Element Msg
viewTable model =
    column
        [ Background.color <| tablecolor
        , padding 100
        , width fill
        , spacing 100
        , height fill
        ]
        --        [ debuggingInformation model
        [ viewTableCards model
        , viewHands model.players
        ]



-- debuggingInformation model =
--     el [] (text <| Debug.toString <| model.x)


viewHands : List Player -> Element Msg
viewHands players =
    row
        [ spacing 100
        , height <| px 135
        , centerX
        ]
        (List.indexedMap viewHand players)


viewHand : Int -> Player -> Element Msg
viewHand index hand =
    column
        [ spacing 10
        , onMouseEnter <| UserHoveredButton index
        , onMouseLeave <| UserUnhoveredButton index
        ]
        [ row [ spacing 10 ] (viewCards hand.cards), el [ centerX ] (text hand.name) ]


viewTableCards : Game -> Element Msg
viewTableCards model =
    row
        [ spacing 10
        , height <| px 135
        , centerX
        ]
        (viewCards <| tableCards model)


viewCards : List Card -> List (Element Msg)
viewCards cards =
    List.map (\c -> viewCard 180 c) cards


viewCard : Int -> Card -> Element Msg
viewCard size card =
    el
        [ Border.rounded 8
        , Font.size size
        , alignTop
        , padding 0
        , spacing 0
        , height <| px (9 * size // 10)
        , Background.color <| rgb255 255 255 255
        , Font.color <| cardColor card
        ]
        (el [ moveUp (0.13 * toFloat size) ]
            (text (cardText card))
        )


cardColor : Card -> Color
cardColor card =
    case card.facing of
        FaceDown ->
            rgb255 103 58 183

        FaceUp ->
            suitToColor card.suit


suitToColor : Suit -> Color
suitToColor suit =
    case suit of
        Heart ->
            rgb255 255 0 0

        Club ->
            rgb255 0 0 0

        Spade ->
            rgb255 0 0 255

        Diamond ->
            rgb255 37 112 37


pokerControls : Element Msg
pokerControls =
    column
        [ Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        , spacing 8
        , padding 8
        ]
        [ Element.text "Game Controls"
        , Input.button
            buttonstyle
            { onPress = Just DoStep
            , label = Element.text "Do"
            }
        ]


buttonstyle : List (Element.Attribute msg)
buttonstyle =
    [ padding 5
    , centerX
    , Border.width 1
    , Border.rounded 3
    , Border.color <| rgb255 200 200 200
    , Font.color <| rgb255 255 255 255
    ]


cardText : Card -> String
cardText card =
    case card.facing of
        FaceDown ->
            "🂠"

        FaceUp ->
            let
                cardLength =
                    String.length "🂱"
            in
            let
                index =
                    cardLength * (suitOffset card.suit + faceOffset card.face - 1)
            in
            String.slice index (index + cardLength) "🂱🂲🂳🂴🂵🂶🂷🂸🂹🂺🂻🂽🂾🂡🂢🂣🂤🂥🂦🂧🂨🂩🂪🂫🂭🂮🃁🃂🃃🃄🃅🃆🃇🃈🃉🃊🃋🃍🃎🃑🃒🃓🃔🃕🃖🃗🃘🃙🃚🃛🃝🃞"


suitOffset : Suit -> Int
suitOffset suit =
    case suit of
        Heart ->
            0

        Spade ->
            13

        Diamond ->
            26

        Club ->
            39


faceOffset : Face -> Int
faceOffset face =
    case face of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13
