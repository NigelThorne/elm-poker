module Main exposing (main)

-- TODO:
-- ✅ deal card from deck
-- ✅ deal random card from deck
-- ✅ shuffle up front
-- ✅ hand per name
-- ✅ start with 2 cards
-- ✅ flop
-- ✅ turn
-- ✅ river
-- ✅ index file -- requires compile (elm make src/Main.elm --output=./main.js)
-- ✅ swap rendering for elm.ui
-- ✅ Shuffle multiple times  (List ShuffleKey)
-- ✅ card colors, table color, card backings
-- ✅ name players
-- ✅ messages in order
-- url ->  join room
-- join room -> url
-- firebase -- share deck
-- detect winner
-- bet money
-- show odds
-- show scoring card
-- union type with one constructor for card and deck.
-- one button does all steps.. dealer presses button...
-- show odds
--
-- Dependencies:
--   elm install elm/random elm/random-extension
--
-- r of n = nPr = n! / (n - r)!
-- nCr = n! / r! (n - r)!
-- score every possible hand in order
---(5) Straight Flush  AKQJT9876 AKQJT9876 AKQJT9876 AKQJT9876  10*4     === 40
---(4) four of a kind  AKQJT98765432   13*(48)                           === 624
---(5) full house XXX,YY  [AAA]4[KK]6 *12 *13                            === 3744
---(5) flush  (5 of 13) * 4 - (Straight Flushs)   nCr = n! / r! (n - r)! === 5108
---(5) straight A[4]K[4]Q[4]J[4]T[4] * 10  - straight flush              === 10200
---(3) three of a kind -- 13*4*(2 of 48) - (full houses)                 === 54912
---(4) two pair -- 13C2 * 11C1 * 4C2 * 4C2 * 4C1                         === 123552
---(2) pair  ((6 * 13) * (3 of 48)) - two-pair - flush - fullhouse       === 1098240
---            13C1 * 12C3 * 4C2 * 4C1 * 4C1 * 4C1
--- high card (5 of 52) - rest                                           === 1302540
-- Total : 2,598,960
-- import Html.Events
--import Element.Background as Background
--import Element.Border as Border
--import Element.Font as Font

import Browser
import Browser.Navigation as Nav
import Data.Chat as Chat
import Data.Firebase as Firebase
import Data.Poker as Poker
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Page.GameDetails exposing (..)
import Page.JoinGame as JoinGamePage exposing (State, viewLobby)
import Ports exposing (..)
import Route
import Styles exposing (..)
import Url
import View.PokerTable


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



{-






                                                               dddddddd
   MMMMMMMM               MMMMMMMM                             d::::::d                   lllllll
   M:::::::M             M:::::::M                             d::::::d                   l:::::l
   M::::::::M           M::::::::M                             d::::::d                   l:::::l
   M:::::::::M         M:::::::::M                             d:::::d                    l:::::l
   M::::::::::M       M::::::::::M   ooooooooooo       ddddddddd:::::d     eeeeeeeeeeee    l::::l
   M:::::::::::M     M:::::::::::M oo:::::::::::oo   dd::::::::::::::d   ee::::::::::::ee  l::::l
   M:::::::M::::M   M::::M:::::::Mo:::::::::::::::o d::::::::::::::::d  e::::::eeeee:::::eel::::l
   M::::::M M::::M M::::M M::::::Mo:::::ooooo:::::od:::::::ddddd:::::d e::::::e     e:::::el::::l
   M::::::M  M::::M::::M  M::::::Mo::::o     o::::od::::::d    d:::::d e:::::::eeeee::::::el::::l
   M::::::M   M:::::::M   M::::::Mo::::o     o::::od:::::d     d:::::d e:::::::::::::::::e l::::l
   M::::::M    M:::::M    M::::::Mo::::o     o::::od:::::d     d:::::d e::::::eeeeeeeeeee  l::::l
   M::::::M     MMMMM     M::::::Mo::::o     o::::od:::::d     d:::::d e:::::::e           l::::l
   M::::::M               M::::::Mo:::::ooooo:::::od::::::ddddd::::::dde::::::::e         l::::::l
   M::::::M               M::::::Mo:::::::::::::::o d:::::::::::::::::d e::::::::eeeeeeee l::::::l
   M::::::M               M::::::M oo:::::::::::oo   d:::::::::ddd::::d  ee:::::::::::::e l::::::l
   MMMMMMMM               MMMMMMMM   ooooooooooo      ddddddddd   ddddd    eeeeeeeeeeeeee llllllll

-}


type alias Model =
    { key : Nav.Key
    , route : Maybe Route.Route
    , state : PageState
    , chat : Chat.Model
    , firebase : Firebase.Model
    , nextRoomName : String
    }


type PageState
    = HomeState
    | PokerGameState GameState
    | JoinGameState JoinGamePage.State


type alias GameState =
    { game : Poker.Game
    , chat : Chat.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , chat = Chat.init { saveMessage = saveMessage }
      , firebase = Firebase.init { signIn = signIn, signOut = signOut }
      , nextRoomName = ""
      , state = initPageState route
      }
    , Cmd.none
    )


initPageState : Maybe Route.Route -> PageState
initPageState route =
    case route of
        Just Route.Home ->
            HomeState

        Just (Route.InGame roomName) ->
            PokerGameState
                { game = Poker.initGame roomName
                , chat = Chat.init { saveMessage = saveMessage }
                }

        Just Route.JoinGame ->
            JoinGameState
                { gameName = "" }

        _ ->
            HomeState



{-







                                                       dddddddd
   UUUUUUUU     UUUUUUUU                               d::::::d                          tttt
   U::::::U     U::::::U                               d::::::d                       ttt:::t
   U::::::U     U::::::U                               d::::::d                       t:::::t
   UU:::::U     U:::::UU                               d:::::d                        t:::::t
    U:::::U     U:::::Uppppp   ppppppppp       ddddddddd:::::d   aaaaaaaaaaaaa  ttttttt:::::ttttttt        eeeeeeeeeeee
    U:::::D     D:::::Up::::ppp:::::::::p    dd::::::::::::::d   a::::::::::::a t:::::::::::::::::t      ee::::::::::::ee
    U:::::D     D:::::Up:::::::::::::::::p  d::::::::::::::::d   aaaaaaaaa:::::at:::::::::::::::::t     e::::::eeeee:::::ee
    U:::::D     D:::::Upp::::::ppppp::::::pd:::::::ddddd:::::d            a::::atttttt:::::::tttttt    e::::::e     e:::::e
    U:::::D     D:::::U p:::::p     p:::::pd::::::d    d:::::d     aaaaaaa:::::a      t:::::t          e:::::::eeeee::::::e
    U:::::D     D:::::U p:::::p     p:::::pd:::::d     d:::::d   aa::::::::::::a      t:::::t          e:::::::::::::::::e
    U:::::D     D:::::U p:::::p     p:::::pd:::::d     d:::::d  a::::aaaa::::::a      t:::::t          e::::::eeeeeeeeeee
    U::::::U   U::::::U p:::::p    p::::::pd:::::d     d:::::d a::::a    a:::::a      t:::::t    tttttte:::::::e
    U:::::::UUU:::::::U p:::::ppppp:::::::pd::::::ddddd::::::dda::::a    a:::::a      t::::::tttt:::::te::::::::e
     UU:::::::::::::UU  p::::::::::::::::p  d:::::::::::::::::da:::::aaaa::::::a      tt::::::::::::::t e::::::::eeeeeeee
       UU:::::::::UU    p::::::::::::::pp    d:::::::::ddd::::d a::::::::::aa:::a       tt:::::::::::tt  ee:::::::::::::e
         UUUUUUUUU      p::::::pppppppp       ddddddddd   ddddd  aaaaaaaaaa  aaaa         ttttttttttt      eeeeeeeeeeeeee
                        p:::::p
                        p:::::p
                       p:::::::p
                       p:::::::p
                       p:::::::p
                       ppppppppp


-}
-- onMouseOver msg =
--   Dom.Element.addAttribute
--     ( "mouseover"
--       |> Dom.Event.action msg
--     )


type Msg
    = GotPokerMsg Poker.Msg
    | ChatMsg Chat.Msg
    | Firebase Firebase.Msg
    | JoinGameMsg JoinGamePage.Msg
    | LeaveRoom
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        GotPokerMsg pmsg ->
            updateModelFromPokerMsg pmsg model

        ChatMsg chatMsg ->
            updateModelFromChatMsg model <| Chat.update chatMsg model.chat model.firebase

        Firebase fmsg ->
            let
                ( a, b ) =
                    Firebase.update fmsg model.firebase 
            in
            ( { model | firebase = a }, Cmd.map Firebase b )

        JoinGameMsg jmsg ->
            case model.state of
                JoinGameState jstate ->
                    updatePageState JoinGameState JoinGameMsg model <| JoinGamePage.update jstate jmsg

                _ ->
                    ( model, Cmd.none )

        -- JoinRoom roomName ->
        --     changeRouteTo (Just (Route.InGame roomName)) model
        --            ( { model | route = Just (Route.InGame model.nextRoomName), game = Just (Poker.initGame model.nextRoomName) }, Cmd.none )
        -- NextRoomNameChanged nextRoomName ->
        --     ( { model | nextRoomName = nextRoomName }, Cmd.none )
        -- LobbyEnterWasPressed ->
        --     changeRouteTo (Just Route.Home) model
        --            ( { model | route = Just (Route.InGame model.nextRoomName), game = Just (Poker.initGame model.nextRoomName) }, Cmd.none )
        LeaveRoom ->
            changeRouteTo (Just Route.Home) model



updatePageState : (a -> PageState) -> (b -> Msg) -> Model -> (a, Cmd b) -> (Model, Cmd Msg)
updatePageState  toPageState toMsg model ( next, cmd )  =
        ( { model | state = toPageState next }, Cmd.map toMsg cmd )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    ( { model | state = initPageState maybeRoute, route = maybeRoute }, Cmd.none )



-- updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
-- updateWith toModel toMsg model ( subModel, subCmd ) =
--     ( toModel subModel
--     , Cmd.map toMsg subCmd
--     )


updateModelFromPokerMsg : Poker.Msg -> Model -> ( Model, Cmd Msg )
updateModelFromPokerMsg msg model =
    case model.state of
        PokerGameState gameState ->
            let
                ( nextGame, cmd ) =
                    Poker.update msg gameState.game
            in
            ( { model | state = PokerGameState { gameState | game = nextGame } }, Cmd.map GotPokerMsg cmd )

        _ ->
            ( model, Cmd.none )



-- updatePageState : Model -> Msg -> (Model, Cmd Msg)
-- updatePageState model msg =
--     let
--         (nextPageState, cmd) =
--     in
--     ({ model | state = nextPageState}, Cmd.map toPageMessage cmd)


updateModelFromChatMsg : Model -> ( Chat.Model, Firebase.Model, Cmd Chat.Msg ) -> ( Model, Cmd Msg )
updateModelFromChatMsg model ( chat, firebase, cmd ) =
    ( { model | chat = chat, firebase = firebase }
    , Cmd.map ChatMsg cmd
    )



{-






                                       bbbbbbbb
      SSSSSSSSSSSSSSS                  b::::::b                                                                      iiii                              tttt            iiii
    SS:::::::::::::::S                 b::::::b                                                                     i::::i                          ttt:::t           i::::i
   S:::::SSSSSS::::::S                 b::::::b                                                                      iiii                           t:::::t            iiii
   S:::::S     SSSSSSS                  b:::::b                                                                                                     t:::::t
   S:::::S            uuuuuu    uuuuuu  b:::::bbbbbbbbb        ssssssssss       ccccccccccccccccrrrrr   rrrrrrrrr  iiiiiiippppp   ppppppppp   ttttttt:::::ttttttt    iiiiiii    ooooooooooo   nnnn  nnnnnnnn        ssssssssss
   S:::::S            u::::u    u::::u  b::::::::::::::bb    ss::::::::::s    cc:::::::::::::::cr::::rrr:::::::::r i:::::ip::::ppp:::::::::p  t:::::::::::::::::t    i:::::i  oo:::::::::::oo n:::nn::::::::nn    ss::::::::::s
    S::::SSSS         u::::u    u::::u  b::::::::::::::::b ss:::::::::::::s  c:::::::::::::::::cr:::::::::::::::::r i::::ip:::::::::::::::::p t:::::::::::::::::t     i::::i o:::::::::::::::on::::::::::::::nn ss:::::::::::::s
     SS::::::SSSSS    u::::u    u::::u  b:::::bbbbb:::::::bs::::::ssss:::::sc:::::::cccccc:::::crr::::::rrrrr::::::ri::::ipp::::::ppppp::::::ptttttt:::::::tttttt     i::::i o:::::ooooo:::::onn:::::::::::::::ns::::::ssss:::::s
       SSS::::::::SS  u::::u    u::::u  b:::::b    b::::::b s:::::s  ssssss c::::::c     ccccccc r:::::r     r:::::ri::::i p:::::p     p:::::p      t:::::t           i::::i o::::o     o::::o  n:::::nnnn:::::n s:::::s  ssssss
          SSSSSS::::S u::::u    u::::u  b:::::b     b:::::b   s::::::s      c:::::c              r:::::r     rrrrrrri::::i p:::::p     p:::::p      t:::::t           i::::i o::::o     o::::o  n::::n    n::::n   s::::::s
               S:::::Su::::u    u::::u  b:::::b     b:::::b      s::::::s   c:::::c              r:::::r            i::::i p:::::p     p:::::p      t:::::t           i::::i o::::o     o::::o  n::::n    n::::n      s::::::s
               S:::::Su:::::uuuu:::::u  b:::::b     b:::::bssssss   s:::::s c::::::c     ccccccc r:::::r            i::::i p:::::p    p::::::p      t:::::t    tttttt i::::i o::::o     o::::o  n::::n    n::::nssssss   s:::::s
   SSSSSSS     S:::::Su:::::::::::::::uub:::::bbbbbb::::::bs:::::ssss::::::sc:::::::cccccc:::::c r:::::r           i::::::ip:::::ppppp:::::::p      t::::::tttt:::::ti::::::io:::::ooooo:::::o  n::::n    n::::ns:::::ssss::::::s
   S::::::SSSSSS:::::S u:::::::::::::::ub::::::::::::::::b s::::::::::::::s  c:::::::::::::::::c r:::::r           i::::::ip::::::::::::::::p       tt::::::::::::::ti::::::io:::::::::::::::o  n::::n    n::::ns::::::::::::::s
   S:::::::::::::::SS   uu::::::::uu:::ub:::::::::::::::b   s:::::::::::ss    cc:::::::::::::::c r:::::r           i::::::ip::::::::::::::pp          tt:::::::::::tti::::::i oo:::::::::::oo   n::::n    n::::n s:::::::::::ss
    SSSSSSSSSSSSSSS       uuuuuuuu  uuuubbbbbbbbbbbbbbbb     sssssssssss        cccccccccccccccc rrrrrrr           iiiiiiiip::::::pppppppp              ttttttttttt  iiiiiiii   ooooooooooo     nnnnnn    nnnnnn  sssssssssss
                                                                                                                           p:::::p
                                                                                                                           p:::::p
                                                                                                                          p:::::::p
                                                                                                                          p:::::::p
                                                                                                                          p:::::::p
                                                                                                                          ppppppppp


-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ChatMsg (receiveMessages Chat.messagesReceived)
        , Sub.map Firebase (signInInfo Firebase.signInInfo)
        , Sub.map Firebase (signInError Firebase.signInError)
        ]



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


view : Model -> Browser.Document Msg
view model =
    { title = "title"
    , body =
        [ layout
            []
          <|
            column [ width fill, height fill ]
                [ row [ centerX, spacing 100 ]
                    [ el [] (text (Route.toString model.route))
                    , link [] { label = text "Home", url = "/home" }
                    , link [] { label = text "NewGame", url = "/poker/newgame" }
                    , link [] { label = text "InGame", url = "/poker/game/test" }
                    ]
                , viewPage model
                ]
        ]
    }


viewPage : Model -> Element Msg
viewPage model =
    case model.state of
        HomeState ->
            viewGameDetailsPage

        PokerGameState gameState ->
            viewInGame gameState model.firebase

        JoinGameState joinState ->
            Element.map JoinGameMsg (JoinGamePage.viewLobby joinState)



-- _ ->
--     text "404 Unknown page"


viewPickUsername : model -> Element Msg
viewPickUsername model =
    text "welcome"


viewInGame : GameState -> Firebase.Model -> Element Msg
viewInGame model firebase =
    row [ height fill, width fill ]
        [ controlPanel model firebase
        , playingArea model.game
        ]


controlPanel : GameState -> Firebase.Model -> Element Msg
controlPanel model firebase =
    column
        [ spacing 20
        , padding 20
        , height fill
        ]
        [ el [ centerX ] (viewSessionControls firebase)
        , el [ centerX ] (Element.map ChatMsg (viewChatWindow model.chat firebase))
        , el [ centerX, alignBottom, Border.width 1, padding 10, Border.rounded 8 ] (Input.button [] { onPress = Just LeaveRoom, label = text "Leave Game" })
        ]


playingArea : Poker.Game -> Element Msg
playingArea game =
    column
        [ height fill
        , width <| fillPortion 5
        ]
        [ -- if Firebase.isSignedIn model.firebase then
          Element.map GotPokerMsg (View.PokerTable.viewTable game)
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
            (Element.map GotPokerMsg View.PokerTable.pokerControls)
        ]


viewSessionControls : Firebase.Model -> Element Msg
viewSessionControls model =
    Element.map (\c -> Firebase c) (viewFirebaseUserControls model)


viewFirebaseUserControls : Firebase.Model -> Element Firebase.Msg
viewFirebaseUserControls model =
    column
        [ width <| px 300
        , spacing 20
        , centerX
        ]
        [ el []
            (case model.userData of
                Just data ->
                    column [ spacing 10, centerX ]
                        [ text "You are logged in as: "
                        , text data.email
                        , Input.button buttonStyle { onPress = Just Firebase.LogOut, label = text "Logout from Google" }
                        ]

                Maybe.Nothing ->
                    column [ spacing 10, centerX ]
                        [ text ""
                        , Input.button buttonStyle { onPress = Just Firebase.LogIn, label = text "Login with Google" }
                        ]
            )
        ]



-- CHAT


viewChatWindow : Chat.Model -> Firebase.Model -> Element Chat.Msg
viewChatWindow model firebase =
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
        , if Firebase.isLoggedIn firebase then
            column [ spacing 10, centerX, width <| px 300 ]
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

          else
            el [] (text "")
        , el [] (text <| Firebase.errorPrinter firebase.error)
        ]
