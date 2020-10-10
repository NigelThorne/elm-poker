module Data.Poker exposing (..)

import Element exposing (..)
import Element.Events exposing (..)
import List.Extra exposing (updateAt)
import Random
import Random.Extra



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
    { uid : String
    , players : List Player
    , deck : Deck
    , community : List Card
    , steps : List Msg
    }


type alias Player =
    { cards : List Card
    , name : String
    }


type alias Deck =
    { cards : List Card }


type alias Card =
    { face : Face
    , suit : Suit
    , facing : Facing
    }


type Face
    = Ace
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


type Status
    = Loading
    | Loaded Deck
    | Errored


type alias ShuffleKey =
    List Int


{-

    Poker: 

    GameState -- describes a game of Poker
    * players 
        each have a seat at the table. 
        each round 
    Chips
    Deck
    player
        stake
        hand
            cards
    seats
    cards
    pot
        round
            hand
            stakes





   Actions :-
   * Deal
   * 
   * Active player  

-}

allSuites : List Suit
allSuites =
    [ Heart, Club, Diamond, Spade ]


allFaces : List Face
allFaces =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


allCardsInSuit : Suit -> List Card
allCardsInSuit suit =
    List.map (\face -> Card face suit FaceDown) allFaces


allCardsInDeck : List Card
allCardsInDeck =
    List.concat (List.map (\suit -> allCardsInSuit suit) allSuites)


newDeck : Deck
newDeck =
    Deck allCardsInDeck


flipCard : Facing -> Card -> Card
flipCard facing card =
    { card | facing = facing }


dealCardToCardList : Facing -> ( List Card, Deck ) -> ( List Card, Deck )
dealCardToCardList facing ( cards, deck ) =
    let
        ( card, deck2 ) =
            removeTopCardFromDeck deck
    in
    ( addCard facing card cards, deck2 )


dealCardsToCardList : ( List Card, Deck ) -> Facing -> Int -> ( List Card, Deck )
dealCardsToCardList ( cards, deck ) facing count =
    case count of
        0 ->
            ( cards, deck )

        n ->
            let
                ( dealt, remains ) =
                    dealCardToCardList facing ( cards, deck )
            in
            dealCardsToCardList ( dealt, remains ) facing (n - 1)


addCard : Facing -> Maybe Card -> List Card -> List Card
addCard facing card cards =
    case card of
        Nothing ->
            cards

        Just c ->
            cards ++ [ { c | facing = facing } ]


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


dealCardsToEachHand : Int -> ( List Player, Deck ) -> ( List Player, Deck )
dealCardsToEachHand count state =
    repeatedly dealACardToEachHand count state


repeatedly : (a -> a) -> Int -> a -> a
repeatedly fn count x =
    case count of
        0 ->
            x

        n ->
            repeatedly fn (n - 1) (fn x)


dealACardToEachHand : ( List Player, Deck ) -> ( List Player, Deck )
dealACardToEachHand ( players, fullDeck ) =
    List.foldl step ( [], fullDeck ) players


step : Player -> ( List Player, Deck ) -> ( List Player, Deck )
step player ( players, deck ) =
    dealCardToPlayer ( player, deck )
        |> (\( p, d ) -> ( players ++ [ p ], d ))


dealCardToPlayer : ( Player, Deck ) -> ( Player, Deck )
dealCardToPlayer ( player, deck ) =
    dealCardToCardList FaceDown ( player.cards, deck )
        |> (\( c, d ) -> ( { player | cards = c }, d ))


shuffleKeyGenerator : Int -> Random.Generator ShuffleKey
shuffleKeyGenerator size =
    List.range 1 size
        |> List.map (\_ -> Random.int 1 size)
        |> Random.Extra.sequence


shuffleDeckWithKeyList : ShuffleKey -> Deck -> Deck
shuffleDeckWithKeyList keylist deck =
    { deck | cards = pickCardListUsingKeyList keylist 0 deck.cards }


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


tableCards : Game -> List Card
tableCards game =
    game.community



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


initHands : List Player
initHands =
    [ Player [] "Bob", Player [] "Jane", Player [] "Freddy" ]


initSteps : List Msg
initSteps =
    [ ShuffleDeck, Flop, Turn, River, ResetTable ]


initGame : String -> Game
initGame id =
    Game id initHands newDeck [] initSteps



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
    | Betting
    | Flop
    | Turn
    | River
    | PayWinnings
    | ShuffleDeckUsingRandomKey ShuffleKey
    | DoStep
    | Noop
    | ResetTable
    | FlipCard Card
    | UserHoveredButton Int
    | UserUnhoveredButton Int


doStep game =
    update (List.head game.steps |> Maybe.withDefault Noop) { game | steps = List.tail game.steps |> Maybe.withDefault [] }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        DoStep ->
            game |> doStep

        ResetTable ->
            ( { game | deck = newDeck, players = initHands, community = [], steps = initSteps }
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
            ( { game | deck = shuffleDeckWithKeyList keylist game.deck } |> dealPlayerCards
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

        Noop ->
            ( game
            , Cmd.none
            )

        FlipCard _ ->
            ( game
            , Cmd.none
            )

        UserHoveredButton index ->
            ( { game | players = flipHand index FaceUp game.players }
            , Cmd.none
            )

        UserUnhoveredButton index ->
            ( { game | players = flipHand index FaceDown game.players }
            , Cmd.none
            )


flipHand : Int -> Facing -> List Player -> List Player
flipHand index facing players =
    updateAt index (\p -> { p | cards = flipCards facing p.cards }) players


flipCards : Facing -> List Card -> List Card
flipCards facing cards =
    List.map (\c -> flipCard facing c) cards


flop : Game -> Game
flop game =
    dealCardsToCommunity 3 game


dealCardsToCommunity : Int -> Game -> Game
dealCardsToCommunity count game =
    let
        ( cards, remains ) =
            dealCardsToCardList ( game.community, game.deck ) FaceUp count
    in
    { game | deck = remains, community = cards }


turn : Game -> Game
turn game =
    dealCardsToCommunity 1 game


river : Game -> Game
river game =
    dealCardsToCommunity 1 game


dealPlayerCards : Game -> Game
dealPlayerCards game =
    let
        ( players, deck ) =
            dealCardsToEachHand 2 ( game.players, game.deck )
    in
    { game | players = players, deck = deck }


dealCardToEachHand : ( List Player, Deck ) -> ( List Player, Deck )
dealCardToEachHand ( players, deck ) =
    case players of
        [] ->
            ( players, deck )

        a :: b ->
            let
                ( h, d ) =
                    dealCardToHand ( a, deck )
            in
            let
                ( hs, d2 ) =
                    dealCardToEachHand ( b, d )
            in
            ( h :: hs, d2 )


dealCardToHand : ( Player, Deck ) -> ( Player, Deck )
dealCardToHand ( hand, deck ) =
    let
        ( h, _ ) =
            dealCardToCardList FaceDown ( hand.cards, deck )
    in
    ( { hand | cards = h }, deck )
