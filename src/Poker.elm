module Poker exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Random
import Random.Extra
import Element.Font exposing (center)


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
type alias Game =
    { hands : List Hand
    , deck : Deck
    , flop : Maybe ( Card, Card, Card )
    , turn : Maybe Card
    , river : Maybe Card
    , x : Maybe ShuffleKey
    }

type alias Hand =
    { cards : List Card
    , player : String
    }


type alias Deck =
    { cards : List Card }

type alias Card =
    { face : Face
    , suit : Suit
    , facing : Facing
    }


type Face = Ace
            | Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
            | Ten
            | Jack
            | Queen
            | King


type Suit
    = Heart
    | Club
    | Diamond
    | Spade


type Facing
    = FaceUp
    | FaceDown




allSuites : List Suit
allSuites =
    [ Heart, Club, Diamond, Spade ]


allFaces : List Face
allFaces =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


allCardsInSuit : Suit -> List Card
allCardsInSuit suit =
    List.map (\face -> Card face suit FaceUp) allFaces


flipCard : Facing -> Card -> Card
flipCard facing card =
    { card | facing = facing }


allCardsInDeck : List Card
allCardsInDeck =
    List.concat (List.map (\suit -> allCardsInSuit suit) allSuites)


newDeck : Deck
newDeck =
    Deck allCardsInDeck

dealCardToCardList : List Card -> Deck -> ( List Card, Deck )
dealCardToCardList cards deck =
    let
        ( card, deck2 ) =
            removeTopCardFromDeck deck
    in
    ( addCard card cards, deck2 )


dealCardsToCardList : List Card -> Deck -> Int -> ( List Card, Deck )
dealCardsToCardList cards deck count =
    case count of
        0 ->
            ( cards, deck )

        n ->
            let
                ( dealt, remains ) =
                    dealCardToCardList cards deck
            in
            dealCardsToCardList dealt remains (n - 1)


addCard : Maybe Card -> List Card -> List Card
addCard card cards =
    case card of
        Nothing ->
            cards

        Just c ->
            cards ++ [ c ]


deckSize : Deck -> Int
deckSize deck =
    List.length deck.cards


removeTopCardFromDeck : Deck -> ( Maybe Card, Deck )
removeTopCardFromDeck deck =
    let
        ( card, rest ) =
            removeTopCardFromList deck.cards
    in
    ( card, { deck | cards = rest } )


removeTopCardFromList : List Card -> ( Maybe Card, List Card )
removeTopCardFromList cards =
    case cards of
        [] ->
            ( Nothing, cards )

        a :: b ->
            ( Just a, b )


removeAnyCardFromCardList : List Card -> Int -> ( Maybe Card, List Card )
removeAnyCardFromCardList cards index =
    let
        offset =
            modBy (List.length cards) index

        header =
            List.take offset cards

        ( card, rest ) =
            removeTopCardFromList (List.drop offset cards)
    in
    ( card, header ++ rest )


dealACardToAHand : Hand -> Deck -> ( Hand, Deck )
dealACardToAHand hand deck =
    let
        ( card, deck2 ) =
            removeTopCardFromDeck deck
    in
    ( { hand | cards = addCard card hand.cards }, deck2 )


dealACardToEachHand : List Hand -> Deck -> ( List Hand, Deck )
dealACardToEachHand hands fullDeck =
    case hands of
        [] ->
            ( [], fullDeck )

        hand :: rest ->
            let
                ( newHand, remainingDeck ) =
                    dealACardToAHand hand fullDeck
            in
            let
                ( dealtHands, restOfDeck ) =
                    dealACardToEachHand rest remainingDeck -- recursive
            in
            ( newHand :: dealtHands, restOfDeck )


type alias ShuffleKey = List Int

shuffleKeyGenerator : Int -> Random.Generator ShuffleKey
shuffleKeyGenerator size =
    List.range 1 size
        |> --     List.reverse |>
           --         List.map (Random.int 1) |>
           --             Random.Extra.sequence
           List.map (\_ -> Random.int 1 size)
        |> Random.Extra.sequence


shuffleDeckWithKeyList : ShuffleKey -> Deck -> Deck
shuffleDeckWithKeyList keylist deck =
    let
        shuffledCards =
            pickCardListUsingKeyList keylist 0 deck.cards
    in
    { deck | cards = shuffledCards }


pickCardListUsingKeyList : List Int -> Int -> List Card -> List Card
pickCardListUsingKeyList keylist offset cards =
    case keylist of
        [] ->
            []

        key :: keysRest ->
            let
                ( card, shortList ) =
                    removeAnyCardFromCardList cards (offset + key)
            in
            case card of
                Nothing ->
                    cards

                Just c ->
                    c :: pickCardListUsingKeyList keysRest (offset + key) shortList



flopCards : Game -> List Card
flopCards model =
    case model.flop of
        Just ( a, b, c ) ->
            [ a, b, c ]

        Nothing ->
            []

tableCards : Game -> List Card
tableCards game =
    (flopCards game)
    ++ (case game.turn of
            Nothing -> []
            Just t -> [t])
    ++ (case game.river of
            Nothing -> []
            Just r -> [r])



{-
 
                                                         
                                                         
 IIIIIIIIII                  iiii          tttt          
 I::::::::I                 i::::i      ttt:::t          
 I::::::::I                  iiii       t:::::t          
 II::::::II                             t:::::t          
   I::::Innnn  nnnnnnnn    iiiiiiittttttt:::::ttttttt    
   I::::In:::nn::::::::nn  i:::::it:::::::::::::::::t    
   I::::In::::::::::::::nn  i::::it:::::::::::::::::t    
   I::::Inn:::::::::::::::n i::::itttttt:::::::tttttt    
   I::::I  n:::::nnnn:::::n i::::i      t:::::t          
   I::::I  n::::n    n::::n i::::i      t:::::t          
   I::::I  n::::n    n::::n i::::i      t:::::t          
   I::::I  n::::n    n::::n i::::i      t:::::t    tttttt
 II::::::IIn::::n    n::::ni::::::i     t::::::tttt:::::t
 I::::::::In::::n    n::::ni::::::i     tt::::::::::::::t
 I::::::::In::::n    n::::ni::::::i       tt:::::::::::tt
 IIIIIIIIIInnnnnn    nnnnnniiiiiiii         ttttttttttt  
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         
 
-}

initRounds : List Msg
initRounds = 
    [ShuffleDeck, DealHands, Betting, Flop, Betting, Turn, Betting, River, Betting, PayWinnings]

initHands : List Hand
initHands =
    [ Hand [] "Bob", Hand [] "Jane", Hand [] "Freddy" ]


initGame : Game
initGame =
    Game initHands newDeck Nothing Nothing Nothing Nothing




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
    = ShuffleDeck
    | DealHands
    | Betting
    | Flop
    | Turn
    | River
    | PayWinnings
    | ShuffleDeckUsingRandomKey ShuffleKey


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        DealHands ->
            ( game |> dealCard |> dealCard
            , Cmd.none
            )

        Flop ->
            ( game |> flop
            , Cmd.none
            )

        Turn ->
            ( game |> turn
            , Cmd.none
            )

        River ->
            ( game |> river
            , Cmd.none
            )

        ShuffleDeck ->
            ( game
            , Random.generate ShuffleDeckUsingRandomKey (shuffleKeyGenerator (deckSize game.deck))
            )

        ShuffleDeckUsingRandomKey keylist ->
            ( { game | deck = shuffleDeckWithKeyList keylist game.deck, x = Just keylist }
            , Cmd.none
            )

        Betting ->
            ( game |> river
            , Cmd.none
            )


        PayWinnings ->
            ( game |> river
            , Cmd.none
            )



flop : Game -> Game
flop game =
    case game.flop of
        Just _ -> game
        Nothing -> 
            let
                ( cards, remains ) =
                    dealCardsToCardList [] game.deck 3
            in
            case cards of
                a :: b :: c :: [] ->
                    { game | flop = Just ( a, b, c ), deck = remains }

                _ ->
                    game

turn : Game -> Game
turn game =
    case game.turn of
        Just _ -> game
        Nothing -> 
            let
                ( card, deck ) =
                    removeTopCardFromDeck game.deck
            in
            { game | turn = card, deck = deck }


river : Game -> Game
river game =
    case game.river of
        Just _ -> game
        Nothing -> 
            let
                ( card, deck ) =
                    removeTopCardFromDeck game.deck
            in
            { game | river = card, deck = deck }


dealCard : Game -> Game
dealCard game =
    let
        ( hands, deck ) =
            dealACardToEachHand game.hands game.deck
    in
    { game | hands = hands, deck = deck }


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


viewTable : Game -> Element msg
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
        , viewHands model.hands
        ]


debuggingInformation model =
    el [] (text <| Debug.toString <| model.x)


viewHands : List Hand -> Element msg
viewHands hands =
    row
        [ spacing 100
        , height <| px 135
        , centerX
        ]
        (List.map viewHand hands)


viewHand : Hand -> Element msg
viewHand hand =
    column [ spacing 10 ] [ row [spacing 10] (viewCards hand.cards), el [ centerX ] (text hand.player) ]


viewTableCards : Game -> Element msg
viewTableCards model =
    row
        [ spacing 10
        , height <| px 135
        , centerX

        --, Element.explain Debug.todo
        ]
        (viewCards <| tableCards model)



viewCards : List Card -> List (Element msg)
viewCards cards =
    List.map viewCard cards



viewCard : Card -> Element msg
viewCard card =
    el
        [ Border.rounded 8
        , Font.size 140
        , alignTop
        , padding 0
        , spacing 0
        , height <| px 128
        , Background.color <| rgb255 255 255 255
        , Font.color <| suitToColor card.suit

        -- , Element.explain Debug.todo
        ]
        (el [moveUp 18]
            (text (cardText card))
        )


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
            { onPress = Just ShuffleDeck
            , label = Element.text "Shuffle"
            }
        , Input.button
            buttonstyle
            { onPress = Just DealHands
            , label = Element.text "Deal Hands"
            }
        , Input.button
            buttonstyle
            { onPress = Just Flop
            , label = Element.text "Flop"
            }
        , Input.button
            buttonstyle
            { onPress = Just Turn
            , label = Element.text "Turn"
            }
        , Input.button
            buttonstyle
            { onPress = Just River
            , label = Element.text "River"
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

color : Card -> String
color card =
    case card.suit of
        Heart ->
            "#ff6d69"

        Diamond ->
            "#decc20"

        Club ->
            "#0ba7bb"

        Spade ->
            "#010b8b"

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
