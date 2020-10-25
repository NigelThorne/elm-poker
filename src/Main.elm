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
-- ✅ url ->  join room
-- load gamestate by room name
-- ✅ join room -> url
-- firebase -- share deck
-- detect winner
-- bet money
-- show odds
-- show scoring card
-- union type with one constructor for card and deck.
-- ✅ one button does all steps.. 
-- only dealer presses button...
-- 
-- Dependencies:
--   elm install elm/random elm/random-extension
--

import Browser
import Browser.Navigation as Nav
import Data.Chat as Chat
import Data.Firebase as Firebase exposing (FirebaseAction(..) )
import Data.Poker as Poker
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Page.GameDetails exposing (..)
import Page.JoinGame as JoinGamePage exposing (State, viewLobby)
import Page.PokerRoom as PokerRoomPage
import Ports exposing (..)
import Route
import Styles exposing (..)
import Url
import View.Chat
import Json.Encode



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
    , firebase : Firebase.Model
    }


type PageState
    = HomeState
    | PokerRoomState PokerRoomPage.GameState
    | JoinGameState JoinGamePage.State



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , firebase = Firebase.init { signIn = signIn, signOut = signOut }
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
            PokerRoomState (PokerRoomPage.initGameState roomName saveMessage)

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


type Msg
    = UrlChanged Url.Url
    | Firebase Firebase.Msg
    | GotPokerMsg PokerRoomPage.Msg
    | ChatMsg Chat.Msg
    | JoinGameMsg JoinGamePage.Msg
    | LeaveRoom
    | LinkClicked Browser.UrlRequest
    | EnterRoomMsg
    | GotPokerRoomInfoMsg Json.Encode.Value 
    


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterRoomMsg -> 
            (model, enterPokerRoom("a5o1Nkfbb4WiHhVN4znw") )

        GotPokerRoomInfoMsg json -> 
            (model, Cmd.none)

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
            case model.state of
                -- PokerGameState pstate ->
                --     PokerRoomPage.update chatMsg model.firebase pstate 
                --         |> \(g,f,m) -> updatePageState PokerGameState ChatMsg {model| firebase = f} (g,m)
                _ ->
                    ( model, Cmd.none )

        Firebase fmsg ->
            let
                ( a, b ) =
                    Firebase.update fmsg model.firebase
            in
            ( { model | firebase = a }, Cmd.map Firebase b )

        JoinGameMsg jmsg ->
            case model.state of
                JoinGameState jstate ->
                    JoinGamePage.update jstate jmsg |> 
                        updatePageState JoinGameState JoinGameMsg model 

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


updatePageState : (a -> PageState) -> (b -> Msg) -> Model -> ( a, Cmd b ) -> ( Model, Cmd Msg )
updatePageState toPageState toMsg model ( next, cmd ) =
    ( { model | state = toPageState next }, Cmd.map toMsg cmd )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    ( { model | state = initPageState maybeRoute, route = maybeRoute }, Cmd.none )


updateModelFromPokerMsg : PokerRoomPage.Msg -> Model -> ( Model, Cmd Msg )
updateModelFromPokerMsg msg model =
    case model.state of
        PokerRoomState gameState ->
            let
                ( nextGame, fb,  cmd ) =
                    PokerRoomPage.update msg model.firebase gameState
            in
                ( { model | state = PokerRoomState nextGame, firebase = fb }, Cmd.map GotPokerMsg cmd )
        
        _ ->
            ( model, Cmd.none )



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
        , pokerRoomInfo GotPokerRoomInfoMsg
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
                    , link [] { label = text "Join Game", url = "/join" }
                    , link [] { label = text "NewGame", url = "/newgame" }
                    , link [] { label = text "InGame", url = "/game/test" }
                    , Input.button [] {onPress = Just EnterRoomMsg , label = text "enterRoom"}
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

        PokerRoomState gameState ->
            viewInGame gameState model.firebase

        JoinGameState joinState ->
            Element.map JoinGameMsg (JoinGamePage.viewLobby joinState)


viewPickUsername : model -> Element Msg
viewPickUsername model =
    text "welcome"



viewInGame : PokerRoomPage.GameState -> Firebase.Model -> Element Msg
viewInGame model firebase =
    case model of 
        PokerRoomPage.Loading gameId -> row [ height fill, width fill ] [text ("loading " ++ gameId)]
        PokerRoomPage.Loaded gameState -> 
            row [ height fill, width fill ]
                [ controlPanel gameState.chat firebase
                , Element.map GotPokerMsg (PokerRoomPage.playingArea gameState.game)
                ]


controlPanel : Chat.Model -> Firebase.Model -> Element Msg
controlPanel model firebase =
    column
        [ spacing 20
        , padding 20
        , height fill
        ]
        [ el [ centerX ] 
                (viewSessionControls firebase)
        , el [ centerX ] 
                (Element.map ChatMsg (View.Chat.viewChatWindow model))
        , el [ centerX, alignBottom, Border.width 1, padding 10, Border.rounded 8 ] 
                (Input.button [] { onPress = Just LeaveRoom, label = text "Leave Game" })
        ]
          
        -- if Firebase.isLoggedIn firebase then
        -- else
        --     el [] (text "")
        -- , el [] (text <| Firebase.errorPrinter firebase.error)




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



-- Poker

enterPokerRoom : String -> Cmd msg
enterPokerRoom roomName =
  firebase <| toRecord (OnSnapshot {path = "/games/"++ roomName ++"", receivePort = "pokerRoomInfo" } )

--Json.Decode.decodeValue logInErrorDecoder >> LoggedInError
