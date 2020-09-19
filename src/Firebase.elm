-- port module Firebase exposing (Model, Msg(..), viewChatWindow, isSignedIn, init, subscriptions, update, viewUserControls)

-- -- import Browser
-- -- import Html exposing (Html, button, div, h1, h2, h3, img, input, p, text)
-- -- import Html.Attributes exposing (placeholder, src, value)
-- -- import Html.Events exposing (onClick, onInput)

-- import Element exposing (..)
-- import Element.Background as Background
-- import Element.Border as Border
-- import Element.Events exposing (..)
-- import Element.Font as Font
-- import Element.Input as Input
-- import Html.Events exposing (onInput)
-- import Json.Decode
-- import Json.Decode.Pipeline
-- import Json.Encode


-- port signIn : () -> Cmd msg
-- port signOut : () -> Cmd msg
-- port signInInfo : (Json.Encode.Value -> msg) -> Sub msg
-- port signInError : (Json.Encode.Value -> msg) -> Sub msg


-- port saveMessage : Json.Encode.Value -> Cmd msg
-- port receiveMessages : (Json.Encode.Value -> msg) -> Sub msg



-- {-
 
                                                                                                
--                                                              dddddddd                           
--  MMMMMMMM               MMMMMMMM                             d::::::d                   lllllll 
--  M:::::::M             M:::::::M                             d::::::d                   l:::::l 
--  M::::::::M           M::::::::M                             d::::::d                   l:::::l 
--  M:::::::::M         M:::::::::M                             d:::::d                    l:::::l 
--  M::::::::::M       M::::::::::M   ooooooooooo       ddddddddd:::::d     eeeeeeeeeeee    l::::l 
--  M:::::::::::M     M:::::::::::M oo:::::::::::oo   dd::::::::::::::d   ee::::::::::::ee  l::::l 
--  M:::::::M::::M   M::::M:::::::Mo:::::::::::::::o d::::::::::::::::d  e::::::eeeee:::::eel::::l 
--  M::::::M M::::M M::::M M::::::Mo:::::ooooo:::::od:::::::ddddd:::::d e::::::e     e:::::el::::l 
--  M::::::M  M::::M::::M  M::::::Mo::::o     o::::od::::::d    d:::::d e:::::::eeeee::::::el::::l 
--  M::::::M   M:::::::M   M::::::Mo::::o     o::::od:::::d     d:::::d e:::::::::::::::::e l::::l 
--  M::::::M    M:::::M    M::::::Mo::::o     o::::od:::::d     d:::::d e::::::eeeeeeeeeee  l::::l 
--  M::::::M     MMMMM     M::::::Mo::::o     o::::od:::::d     d:::::d e:::::::e           l::::l 
--  M::::::M               M::::::Mo:::::ooooo:::::od::::::ddddd::::::dde::::::::e         l::::::l
--  M::::::M               M::::::Mo:::::::::::::::o d:::::::::::::::::d e::::::::eeeeeeee l::::::l
--  M::::::M               M::::::M oo:::::::::::oo   d:::::::::ddd::::d  ee:::::::::::::e l::::::l
--  MMMMMMMM               MMMMMMMM   ooooooooooo      ddddddddd   ddddd    eeeeeeeeeeeeee llllllll
                                                                                                
                                                                                                
                                                                                                
                                                                                                
                                                                                                
                                                                                                
                                                                                                
 
-- -}

-- type alias ErrorData =
--     { code : Maybe String
--     , message : Maybe String
--     , credential : Maybe String
--     }


-- type alias UserData =
--     { token : String
--     , email : String
--     , uid : String
--     }


-- type alias Model =
--     { userData : Maybe UserData
--     , error : ErrorData
--     , inputContent : String
--     , messages : List String
--     }

-- {-
 
                                                         
                                                         
--  IIIIIIIIII                  iiii          tttt          
--  I::::::::I                 i::::i      ttt:::t          
--  I::::::::I                  iiii       t:::::t          
--  II::::::II                             t:::::t          
--    I::::Innnn  nnnnnnnn    iiiiiiittttttt:::::ttttttt    
--    I::::In:::nn::::::::nn  i:::::it:::::::::::::::::t    
--    I::::In::::::::::::::nn  i::::it:::::::::::::::::t    
--    I::::Inn:::::::::::::::n i::::itttttt:::::::tttttt    
--    I::::I  n:::::nnnn:::::n i::::i      t:::::t          
--    I::::I  n::::n    n::::n i::::i      t:::::t          
--    I::::I  n::::n    n::::n i::::i      t:::::t          
--    I::::I  n::::n    n::::n i::::i      t:::::t    tttttt
--  II::::::IIn::::n    n::::ni::::::i     t::::::tttt:::::t
--  I::::::::In::::n    n::::ni::::::i     tt::::::::::::::t
--  I::::::::In::::n    n::::ni::::::i       tt:::::::::::tt
--  IIIIIIIIIInnnnnn    nnnnnniiiiiiii         ttttttttttt  
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
 
-- -}

-- init : Model
-- init =
--     { userData = Maybe.Nothing, error = emptyError, inputContent = "", messages = [] }

-- isSignedIn : Model -> Bool 
-- isSignedIn model =
--     case model.userData of
--         Nothing -> False
--         _ -> True

-- {-
 
                                                                                                                         
--                                                      dddddddd                                                            
--  UUUUUUUU     UUUUUUUU                               d::::::d                          tttt                              
--  U::::::U     U::::::U                               d::::::d                       ttt:::t                              
--  U::::::U     U::::::U                               d::::::d                       t:::::t                              
--  UU:::::U     U:::::UU                               d:::::d                        t:::::t                              
--   U:::::U     U:::::Uppppp   ppppppppp       ddddddddd:::::d   aaaaaaaaaaaaa  ttttttt:::::ttttttt        eeeeeeeeeeee    
--   U:::::D     D:::::Up::::ppp:::::::::p    dd::::::::::::::d   a::::::::::::a t:::::::::::::::::t      ee::::::::::::ee  
--   U:::::D     D:::::Up:::::::::::::::::p  d::::::::::::::::d   aaaaaaaaa:::::at:::::::::::::::::t     e::::::eeeee:::::ee
--   U:::::D     D:::::Upp::::::ppppp::::::pd:::::::ddddd:::::d            a::::atttttt:::::::tttttt    e::::::e     e:::::e
--   U:::::D     D:::::U p:::::p     p:::::pd::::::d    d:::::d     aaaaaaa:::::a      t:::::t          e:::::::eeeee::::::e
--   U:::::D     D:::::U p:::::p     p:::::pd:::::d     d:::::d   aa::::::::::::a      t:::::t          e:::::::::::::::::e 
--   U:::::D     D:::::U p:::::p     p:::::pd:::::d     d:::::d  a::::aaaa::::::a      t:::::t          e::::::eeeeeeeeeee  
--   U::::::U   U::::::U p:::::p    p::::::pd:::::d     d:::::d a::::a    a:::::a      t:::::t    tttttte:::::::e           
--   U:::::::UUU:::::::U p:::::ppppp:::::::pd::::::ddddd::::::dda::::a    a:::::a      t::::::tttt:::::te::::::::e          
--    UU:::::::::::::UU  p::::::::::::::::p  d:::::::::::::::::da:::::aaaa::::::a      tt::::::::::::::t e::::::::eeeeeeee  
--      UU:::::::::UU    p::::::::::::::pp    d:::::::::ddd::::d a::::::::::aa:::a       tt:::::::::::tt  ee:::::::::::::e  
--        UUUUUUUUU      p::::::pppppppp       ddddddddd   ddddd  aaaaaaaaaa  aaaa         ttttttttttt      eeeeeeeeeeeeee  
--                       p:::::p                                                                                            
--                       p:::::p                                                                                            
--                      p:::::::p                                                                                           
--                      p:::::::p                                                                                           
--                      p:::::::p                                                                                           
--                      ppppppppp                                                                                           
                                                                                                                         
 
-- -}


-- type Msg
--     = LogIn
--     | LogOut
--     | LoggedInData (Result Json.Decode.Error UserData)
--     | LoggedInError (Result Json.Decode.Error ErrorData)
--     | SaveMessage
--     | InputChanged String
--     | MessagesReceived (Result Json.Decode.Error (List String))
--     | EnterWasPressed


-- emptyError : ErrorData
-- emptyError =
--     { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }


-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         LogIn ->
--             ( model, signIn () )

--         LogOut ->
--             ( { model | userData = Maybe.Nothing, error = emptyError }, signOut () )

--         LoggedInData result ->
--             case result of
--                 Ok value ->
--                     ( { model | userData = Just value }, Cmd.none )

--                 Err error ->
--                     ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

--         LoggedInError result ->
--             case result of
--                 Ok value ->
--                     ( { model | error = value }, Cmd.none )

--                 Err error ->
--                     ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

--         SaveMessage ->
--             ( {model | inputContent = ""}, saveMessage <| messageEncoder model )

--         InputChanged value ->
--             ( { model | inputContent = value }, Cmd.none )

--         MessagesReceived result ->
--             case result of
--                 Ok value ->
--                     ( { model | messages = value }, Cmd.none )

--                 Err error ->
--                     ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

--         EnterWasPressed -> 
--             ( {model | inputContent = ""}, saveMessage <| messageEncoder model)

-- onEnter : msg -> Element.Attribute msg
-- onEnter msg =
--     Element.htmlAttribute
--         (Html.Events.on "keyup"
--             (Json.Decode.field "key" Json.Decode.string
--                 |> Json.Decode.andThen
--                     (\key ->
--                         if key == "Enter" then
--                             Json.Decode.succeed msg

--                         else
--                             Json.Decode.fail "Not the enter key"
--                     )
--             )
--         )

-- messageEncoder : Model -> Json.Encode.Value
-- messageEncoder model =
--     Json.Encode.object
--         [ ( "content", Json.Encode.string model.inputContent )
--         , ( "uid"
--           , case model.userData of
--                 Just userData ->
--                     Json.Encode.string userData.uid

--                 Maybe.Nothing ->
--                     Json.Encode.null
--           )
--         ]


-- messageToError : String -> ErrorData
-- messageToError message =
--     { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


-- errorPrinter : ErrorData -> String
-- errorPrinter errorData =
--     Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message


-- userDataDecoder : Json.Decode.Decoder UserData
-- userDataDecoder =
--     Json.Decode.succeed UserData
--         |> Json.Decode.Pipeline.required "token" Json.Decode.string
--         |> Json.Decode.Pipeline.required "email" Json.Decode.string
--         |> Json.Decode.Pipeline.required "uid" Json.Decode.string


-- logInErrorDecoder : Json.Decode.Decoder ErrorData
-- logInErrorDecoder =
--     Json.Decode.succeed ErrorData
--         |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
--         |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
--         |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)


-- messagesDecoder =
--     Json.Decode.decodeString (Json.Decode.list Json.Decode.string)


-- messageListDecoder : Json.Decode.Decoder (List String)
-- messageListDecoder =
--     Json.Decode.succeed identity
--         |> Json.Decode.Pipeline.required "messages" (Json.Decode.list Json.Decode.string)



-- {-
 
                                                                                              
                                                                                              
--  VVVVVVVV           VVVVVVVV iiii                                                             
--  V::::::V           V::::::Vi::::i                                                            
--  V::::::V           V::::::V iiii                                                             
--  V::::::V           V::::::V                                                                  
--   V:::::V           V:::::Viiiiiii     eeeeeeeeeeee  wwwwwww           wwwww           wwwwwww
--    V:::::V         V:::::V i:::::i   ee::::::::::::ee w:::::w         w:::::w         w:::::w 
--     V:::::V       V:::::V   i::::i  e::::::eeeee:::::eew:::::w       w:::::::w       w:::::w  
--      V:::::V     V:::::V    i::::i e::::::e     e:::::e w:::::w     w:::::::::w     w:::::w   
--       V:::::V   V:::::V     i::::i e:::::::eeeee::::::e  w:::::w   w:::::w:::::w   w:::::w    
--        V:::::V V:::::V      i::::i e:::::::::::::::::e    w:::::w w:::::w w:::::w w:::::w     
--         V:::::V:::::V       i::::i e::::::eeeeeeeeeee      w:::::w:::::w   w:::::w:::::w      
--          V:::::::::V        i::::i e:::::::e                w:::::::::w     w:::::::::w       
--           V:::::::V        i::::::ie::::::::e                w:::::::w       w:::::::w        
--            V:::::V         i::::::i e::::::::eeeeeeee         w:::::w         w:::::w         
--             V:::V          i::::::i  ee:::::::::::::e          w:::w           w:::w          
--              VVV           iiiiiiii    eeeeeeeeeeeeee           www             www           
                                                                                              
                                                                                              
                                                                                              
                                                                                              
                                                                                              
                                                                                              
                                                                                              
 
-- -}

-- buttonStyle =   [ height fill
--         , width <| fillPortion 1
--         , Background.color <| rgb255 92 99 118
--         , Font.color <| rgb255 255 255 255
--         , spacing 8
--         , padding 8
--         ]


-- viewUserControls : Model -> Element Msg
-- viewUserControls model =
--     column 
--         [ width <| px 300
--         , spacing 20
--         , centerX
--         ]
--         [ el []
--             (
--                 case model.userData of
--                     Just data ->
--                         column [spacing 10, centerX] 
--                         [ text "You are logged in as: "
--                         , text data.email
--                         , Input.button buttonStyle { onPress = Just LogOut, label = text "Logout from Google" }
--                         ]

--                     Maybe.Nothing ->
--                         column [spacing 10, centerX] 
--                         [ text ""
--                         , Input.button buttonStyle { onPress = Just LogIn, label = text "Login with Google" }
--                         ]
--             )
--         ]

-- viewChatWindow : Model -> Element Msg
-- viewChatWindow model =
--     column 
--         [ width <| px 300
--         , spacing 20 
--         , centerX ]
--         [ 
--          case model.userData of
--             Just _ ->
--                 column [spacing 10, centerX ]
--                     [ Input.text
--                         [ onEnter EnterWasPressed ]
--                         { label = Input.labelAbove [] (text "Message to send")
--                         , onChange = InputChanged
--                         , placeholder = Nothing -- Just (Input.placeholder [] (text ""))
--                         , text = model.inputContent
--                         }
--                     , Input.button
--                         buttonStyle
--                         { onPress = Just SaveMessage
--                         , label = text "Save new message"
--                         }
--                     ]

--             Maybe.Nothing ->
--                 el [] (text "")
--         , column [centerX]
--             [ column 
--                 [ spacing 20
--                 , Border.solid
--                 , Border.width 1
--                 , padding 10
--                 ]
--                 [ text "Previous messages"
--                 , column [] <|
--                     List.map
--                         (\m -> paragraph [] [ text m ])
--                         model.messages
--                 ]
--             ]
--         , el [] (text <| errorPrinter model.error)
--         ]


-- {-
 
                                                                                                                                                                                                                               
--                                      bbbbbbbb                                                                                                                                                                                  
--     SSSSSSSSSSSSSSS                  b::::::b                                                                      iiii                              tttt            iiii                                                      
--   SS:::::::::::::::S                 b::::::b                                                                     i::::i                          ttt:::t           i::::i                                                     
--  S:::::SSSSSS::::::S                 b::::::b                                                                      iiii                           t:::::t            iiii                                                      
--  S:::::S     SSSSSSS                  b:::::b                                                                                                     t:::::t                                                                      
--  S:::::S            uuuuuu    uuuuuu  b:::::bbbbbbbbb        ssssssssss       ccccccccccccccccrrrrr   rrrrrrrrr  iiiiiiippppp   ppppppppp   ttttttt:::::ttttttt    iiiiiii    ooooooooooo   nnnn  nnnnnnnn        ssssssssss   
--  S:::::S            u::::u    u::::u  b::::::::::::::bb    ss::::::::::s    cc:::::::::::::::cr::::rrr:::::::::r i:::::ip::::ppp:::::::::p  t:::::::::::::::::t    i:::::i  oo:::::::::::oo n:::nn::::::::nn    ss::::::::::s  
--   S::::SSSS         u::::u    u::::u  b::::::::::::::::b ss:::::::::::::s  c:::::::::::::::::cr:::::::::::::::::r i::::ip:::::::::::::::::p t:::::::::::::::::t     i::::i o:::::::::::::::on::::::::::::::nn ss:::::::::::::s 
--    SS::::::SSSSS    u::::u    u::::u  b:::::bbbbb:::::::bs::::::ssss:::::sc:::::::cccccc:::::crr::::::rrrrr::::::ri::::ipp::::::ppppp::::::ptttttt:::::::tttttt     i::::i o:::::ooooo:::::onn:::::::::::::::ns::::::ssss:::::s
--      SSS::::::::SS  u::::u    u::::u  b:::::b    b::::::b s:::::s  ssssss c::::::c     ccccccc r:::::r     r:::::ri::::i p:::::p     p:::::p      t:::::t           i::::i o::::o     o::::o  n:::::nnnn:::::n s:::::s  ssssss 
--         SSSSSS::::S u::::u    u::::u  b:::::b     b:::::b   s::::::s      c:::::c              r:::::r     rrrrrrri::::i p:::::p     p:::::p      t:::::t           i::::i o::::o     o::::o  n::::n    n::::n   s::::::s      
--              S:::::Su::::u    u::::u  b:::::b     b:::::b      s::::::s   c:::::c              r:::::r            i::::i p:::::p     p:::::p      t:::::t           i::::i o::::o     o::::o  n::::n    n::::n      s::::::s   
--              S:::::Su:::::uuuu:::::u  b:::::b     b:::::bssssss   s:::::s c::::::c     ccccccc r:::::r            i::::i p:::::p    p::::::p      t:::::t    tttttt i::::i o::::o     o::::o  n::::n    n::::nssssss   s:::::s 
--  SSSSSSS     S:::::Su:::::::::::::::uub:::::bbbbbb::::::bs:::::ssss::::::sc:::::::cccccc:::::c r:::::r           i::::::ip:::::ppppp:::::::p      t::::::tttt:::::ti::::::io:::::ooooo:::::o  n::::n    n::::ns:::::ssss::::::s
--  S::::::SSSSSS:::::S u:::::::::::::::ub::::::::::::::::b s::::::::::::::s  c:::::::::::::::::c r:::::r           i::::::ip::::::::::::::::p       tt::::::::::::::ti::::::io:::::::::::::::o  n::::n    n::::ns::::::::::::::s 
--  S:::::::::::::::SS   uu::::::::uu:::ub:::::::::::::::b   s:::::::::::ss    cc:::::::::::::::c r:::::r           i::::::ip::::::::::::::pp          tt:::::::::::tti::::::i oo:::::::::::oo   n::::n    n::::n s:::::::::::ss  
--   SSSSSSSSSSSSSSS       uuuuuuuu  uuuubbbbbbbbbbbbbbbb     sssssssssss        cccccccccccccccc rrrrrrr           iiiiiiiip::::::pppppppp              ttttttttttt  iiiiiiii   ooooooooooo     nnnnnn    nnnnnn  sssssssssss    
--                                                                                                                          p:::::p                                                                                               
--                                                                                                                          p:::::p                                                                                               
--                                                                                                                         p:::::::p                                                                                              
--                                                                                                                         p:::::::p                                                                                              
--                                                                                                                         p:::::::p                                                                                              
--                                                                                                                         ppppppppp                                                                                              
                                                                                                                                                                                                                               
 
-- -}


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.batch
--         [ signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
--         , signInError (Json.Decode.decodeValue logInErrorDecoder >> LoggedInError)
--         , receiveMessages (Json.Decode.decodeValue messageListDecoder >> MessagesReceived)
--         ]
