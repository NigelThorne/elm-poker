module Page.PokerRoom exposing (..)



import Random
import Data.Poker as Poker
import Data.Chat as Chat
import Data.Firebase as Firebase
import Element exposing (Element)
import Data.Poker exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Styles exposing (..)
import Data.Cards as Cards exposing (Card, ShuffleKey, shuffleKeyGenerator, shuffleDeck, deckSize, Facing(..), newDeck)
import Json.Encode


      -- let
      --     ( chat, firebase, cmd ) =
      --         Chat.update chatMsg pstate.chat model.firebase
      -- in
      -- ( { model | state = PokerGameState { pstate | chat = chat }, firebase = firebase }, Cmd.map ChatMsg cmd )


type GameState
      = Loading String
      | Loaded LoadedGameState

initGameState: String -> (Json.Encode.Value -> Cmd msg) -> GameState
initGameState roomName chatPort = 
    Loading roomName

type alias LoadedGameState =
      { game : Poker.Game
      , chat : Chat.Model
      --, steps : List Msg
      }

type Msg
    = Noop
    | ShuffleDeck
    | Betting
    | Flop
    | Turn
    | River
    | PayWinnings
    | ShuffleDeckUsingKey ShuffleKey
    | DoStep
    | ResetTable
    | FlipCard Card
    | UserHoveredButton Int
    | UserUnhoveredButton Int
    -- GotLoaded

-- doStep : { a | steps : List Msg } -> ( Game, Cmd Msg )
-- doStep game =
--     update 
--         (List.head game.steps |> Maybe.withDefault Noop) 
--         { game | steps = (List.tail game.steps) |> Maybe.withDefault [] }


update : Msg -> Firebase.Model -> GameState -> ( GameState, Firebase.Model, Cmd Msg )
update msg firebase gameState =
    case gameState of
        Loaded lgs -> updateLoaded msg firebase lgs.game  |> \(gs, fb, cmd) -> (Loaded {lgs | game=gs}, fb, cmd) 
        _ -> (gameState, firebase, Cmd.none)

updateLoaded : Msg -> Firebase.Model -> Game -> ( Game, Firebase.Model, Cmd Msg )
updateLoaded msg firebase game =
    case msg of
        DoStep ->
            (game, firebase, Cmd.none) -- |> doStep

        ResetTable ->
            ( Poker.clearTable game
            , firebase
            , Cmd.none
            )

        Flop ->
            ( game |> flop
            , firebase
            , Cmd.none
            )

        Turn ->
            ( game |> turn
            , firebase
            , Cmd.none
            )

        River ->
            ( game |> river
            , firebase
            , Cmd.none
            )

        ShuffleDeck ->
            ( game
            , firebase
            , Random.generate ShuffleDeckUsingKey (shuffleKeyGenerator (deckSize game.deck))
            )

        ShuffleDeckUsingKey keylist ->
            ( { game | deck = shuffleDeck keylist game.deck } 
            , firebase
            , Cmd.none
            )

        Betting ->
            ( game |> river
            , firebase
            , Cmd.none
            )

        PayWinnings ->
            ( game |> river
            , firebase
            , Cmd.none
            )

        Noop ->
            ( game
            , firebase
            , Cmd.none
            )

        FlipCard _ ->
            ( game
            , firebase
            , Cmd.none
            )

        UserHoveredButton index ->
            ( { game | players = flipHand index FaceUp game.players }
            , firebase
            , Cmd.none
            )

        UserUnhoveredButton index ->
            ( { game | players = flipHand index FaceDown game.players }
            , firebase
            , Cmd.none
            )


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


playingArea : Game -> Element Msg
playingArea game =
    column
        [ height fill
        , width <| fillPortion 5
        ]
        [ -- if Firebase.isSignedIn model.firebase then
          viewTable game
        , el
            [ width fill
            , height fill
            , Background.color <| rgb255 0 123 23
            ]
            Element.none
        , el
            [ width fill
            , Background.color <| rgb255 123 123 123
            ]
            pokerControls
        ]


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
viewHand index player =
    column
        [ spacing 10
        , onMouseEnter <| UserHoveredButton index
        , onMouseLeave <| UserUnhoveredButton index
        ]
        [ row [ spacing 10 ] (viewCards player.hand), el [ centerX ] (text player.name) ]


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
        , Font.color <| Cards.cardColor card
        ]
        (el [ moveUp (0.13 * toFloat size) ]
            (text (Cards.toString card))
        )




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
            , label = Element.text "Deal"
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

