module Main exposing (main)


-- TODO:
-- ✅ deal card from deck
-- ✅ deal random card from deck
-- ✅ shuffle up front
-- ✅ hand per player
-- ✅ start with 2 cards
-- ✅ flop
-- ✅ turn
-- ✅ river
-- ✅ index file -- requires compile (elm make src/Main.elm --output=./main.js)
-- ✅ swap rendering for elm.ui
-- union type with one constructor for card and deck.
-- ✅ Shuffle multiple times  (List ShuffleKey)
-- firebase -- share deck
-- ✅ card colors, table color, card backings
-- ✅ name players
-- detect winner
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
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (..)
import Chat
import Html
import Poker as Poker
import Firebase 



{-
 
                                                                           
                                                                           
 MMMMMMMM               MMMMMMMM                    iiii                   
 M:::::::M             M:::::::M                   i::::i                  
 M::::::::M           M::::::::M                    iiii                   
 M:::::::::M         M:::::::::M                                           
 M::::::::::M       M::::::::::M  aaaaaaaaaaaaa   iiiiiiinnnn  nnnnnnnn    
 M:::::::::::M     M:::::::::::M  a::::::::::::a  i:::::in:::nn::::::::nn  
 M:::::::M::::M   M::::M:::::::M  aaaaaaaaa:::::a  i::::in::::::::::::::nn 
 M::::::M M::::M M::::M M::::::M           a::::a  i::::inn:::::::::::::::n
 M::::::M  M::::M::::M  M::::::M    aaaaaaa:::::a  i::::i  n:::::nnnn:::::n
 M::::::M   M:::::::M   M::::::M  aa::::::::::::a  i::::i  n::::n    n::::n
 M::::::M    M:::::M    M::::::M a::::aaaa::::::a  i::::i  n::::n    n::::n
 M::::::M     MMMMM     M::::::Ma::::a    a:::::a  i::::i  n::::n    n::::n
 M::::::M               M::::::Ma::::a    a:::::a i::::::i n::::n    n::::n
 M::::::M               M::::::Ma:::::aaaa::::::a i::::::i n::::n    n::::n
 M::::::M               M::::::M a::::::::::aa:::ai::::::i n::::n    n::::n
 MMMMMMMM               MMMMMMMM  aaaaaaaaaa  aaaaiiiiiiii nnnnnn    nnnnnn
                                                                           
-}

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
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
    { game : Poker.Game
    , firebase : Chat.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Poker.initGame Chat.init
    , Cmd.none
    )



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
    = PokerMsg Poker.Msg
    | FirebaseMsg Chat.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PokerMsg pmsg ->
            updateModelFromPokerMsg model <| Poker.update pmsg model.game

        FirebaseMsg fmsg ->
            updateModelFromFirebaseMsg model <| Chat.update fmsg model.firebase


updateModelFromPokerMsg : Model -> ( Poker.Game, Cmd Poker.Msg ) -> ( Model, Cmd Msg )
updateModelFromPokerMsg model ( game, cmd ) =
    ( { model | game = game }
    , Cmd.map mapPokerMsg cmd
    )


updateModelFromFirebaseMsg : Model -> ( Chat.Model, Cmd Chat.Msg ) -> ( Model, Cmd Msg )
updateModelFromFirebaseMsg model ( firebase, cmd ) =
    ( { model | firebase = firebase }
    , Cmd.map mapFirebaseMsg cmd
    )


mapPokerMsg : Poker.Msg -> Msg
mapPokerMsg pokerMsg =
    PokerMsg pokerMsg


mapFirebaseMsg : Chat.Msg -> Msg
mapFirebaseMsg firebaseMsg =
    FirebaseMsg firebaseMsg



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
    Sub.map mapFirebaseMsg (Chat.subscriptions model.firebase)



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


view : Model -> Html.Html Msg
view model =
    layout [] <|
        row [ height fill, width fill ]
            [ controlPanel model
            , playingArea model
            ]


controlPanel : Model -> Element Msg
controlPanel model =
    column 
        [ spacing 100
        , padding 20
        , height fill
        ] 
        [ el [centerX] (Element.map mapFirebaseMsg (Chat.viewUserControls model.firebase))
        , el [centerX] (Element.map mapFirebaseMsg (Chat.viewChatWindow model.firebase))
        ]

playingArea : Model -> Element Msg
playingArea model =
    column
        [ height fill
        , width <| fillPortion 5
        ]
        [ 
            if Firebase.isSignedIn model.firebase.firebase then
                Poker.viewTable model.game
            else
               Element.map mapFirebaseMsg (Chat.viewUserControls model.firebase)
        , el [width fill  
            , Background.color <| rgb255 0 123 23
            ]
            (Element.map mapPokerMsg Poker.pokerControls )
        ]
