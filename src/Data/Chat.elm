module Data.Chat exposing (Model, Msg(..), init, messagesReceived, update)

import Data.Firebase as Firebase
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode



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
    { inputContent : String
    , messages : List Message
    , saveMessage : Json.Encode.Value -> Cmd Msg
    }


type alias Message =
    { text : String
    , timestamp : Int
    }


init : { saveMessage : Json.Encode.Value -> Cmd Msg } -> Model
init args =
    { inputContent = "", messages = [], saveMessage = args.saveMessage }

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
    = SaveMessage
    | InputChanged String
    | MessagesReceived (Result Json.Decode.Error (List Message))
    | EnterWasPressed


update : Msg -> Model -> Firebase.Model -> ( Model, Firebase.Model, Cmd Msg )
update msg model firebase =
    case msg of
        SaveMessage ->
            ( { model | inputContent = "" }, firebase, model.saveMessage <| messageEncoder model firebase )

        InputChanged value ->
            ( { model | inputContent = value }, firebase, Cmd.none )

        MessagesReceived result ->
            case result of
                Ok value ->
                    ( { model | messages = value |> List.sortBy (\v -> -v.timestamp) }, firebase, Cmd.none )

                Err error ->
                    ( model, Firebase.setError firebase error, Cmd.none )

        EnterWasPressed ->
            ( { model | inputContent = "" }, firebase, model.saveMessage <| messageEncoder model firebase )



{-
 
                                                                                    
                                                                                    
           JJJJJJJJJJJ   SSSSSSSSSSSSSSS      OOOOOOOOO     NNNNNNNN        NNNNNNNN
           J:::::::::J SS:::::::::::::::S   OO:::::::::OO   N:::::::N       N::::::N
           J:::::::::JS:::::SSSSSS::::::S OO:::::::::::::OO N::::::::N      N::::::N
           JJ:::::::JJS:::::S     SSSSSSSO:::::::OOO:::::::ON:::::::::N     N::::::N
             J:::::J  S:::::S            O::::::O   O::::::ON::::::::::N    N::::::N
             J:::::J  S:::::S            O:::::O     O:::::ON:::::::::::N   N::::::N
             J:::::J   S::::SSSS         O:::::O     O:::::ON:::::::N::::N  N::::::N
             J:::::j    SS::::::SSSSS    O:::::O     O:::::ON::::::N N::::N N::::::N
             J:::::J      SSS::::::::SS  O:::::O     O:::::ON::::::N  N::::N:::::::N
 JJJJJJJ     J:::::J         SSSSSS::::S O:::::O     O:::::ON::::::N   N:::::::::::N
 J:::::J     J:::::J              S:::::SO:::::O     O:::::ON::::::N    N::::::::::N
 J::::::J   J::::::J              S:::::SO::::::O   O::::::ON::::::N     N:::::::::N
 J:::::::JJJ:::::::J  SSSSSSS     S:::::SO:::::::OOO:::::::ON::::::N      N::::::::N
  JJ:::::::::::::JJ   S::::::SSSSSS:::::S OO:::::::::::::OO N::::::N       N:::::::N
    JJ:::::::::JJ     S:::::::::::::::SS    OO:::::::::OO   N::::::N        N::::::N
      JJJJJJJJJ        SSSSSSSSSSSSSSS        OOOOOOOOO     NNNNNNNN         NNNNNNN
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                                    
                                                                                    
 
-}

messageEncoder : Model -> Firebase.Model -> Json.Encode.Value
messageEncoder model firebase =
    Firebase.messageEncoder firebase model.inputContent


messageDecoder : Json.Decode.Decoder Message
messageDecoder =
    Json.Decode.succeed Message
        |> Json.Decode.Pipeline.required "text" Json.Decode.string
        |> Json.Decode.Pipeline.required "timestamp" Json.Decode.int


messageListDecoder : Json.Decode.Decoder (List Message)
messageListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "messages" (Json.Decode.list messageDecoder)


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


messagesReceived : Json.Encode.Value -> Msg
messagesReceived =
    Json.Decode.decodeValue messageListDecoder >> MessagesReceived
