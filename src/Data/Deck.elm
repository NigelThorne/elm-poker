module Data.Deck exposing (Deck, Card, ShuffleKey, Facing(..), toString, cardColor, dealToAll, addCard, times, deal, emptyDeck, shuffleKeyGenerator, shuffleDeck, flipCards, newDeck, deckSize, flipCard)

import List.Extra exposing (updateAt)
import Random
import Random.Extra
import Element exposing (Color)
import Element exposing (rgb255)

type alias Deck =
    List Card

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


type alias ShuffleKey =
    List Int

newDeck : Deck
newDeck =
    allCardsInDeck

emptyDeck : Deck
emptyDeck = 
  []

toString : Card -> String
toString card =
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


allSuites : List Suit
allSuites =
    [ Heart, Club, Diamond, Spade ]


allFaces : List Face
allFaces =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


allCardsInSuit : Suit -> Deck
allCardsInSuit suit =
    List.map (\face -> Card face suit FaceDown) allFaces

allCardsInDeck : Deck
allCardsInDeck =
    List.concat (List.map (\suit -> allCardsInSuit suit) allSuites)

flipCard : Facing -> Card -> Card
flipCard facing card =
    { card | facing = facing }



addCard : Facing -> Maybe Card -> Deck -> Deck
addCard f card cards =
    case card of
        Nothing ->
            cards

        Just c ->
            cards ++ [ { c | facing = f } ]


deckSize : Deck -> Int
deckSize deck =
    List.length deck



removeTopCard : Deck -> ( Maybe Card, Deck )
removeTopCard cards =
    case cards of
        [] ->
            ( Nothing, cards )

        a :: b ->
            ( Just a, b )

removeAnyCard : Int -> Deck -> ( Maybe Card, Deck )
removeAnyCard index cards =
    let
        offset =
            modBy (List.length cards) index

        header =
            List.take offset cards

        ( card, rest ) =
            removeTopCard (List.drop offset cards)
    in
    ( card, header ++ rest )

shuffleKeyGenerator : Int -> Random.Generator ShuffleKey
shuffleKeyGenerator size =
    List.range 1 size
        |> List.map (\_ -> Random.int 1 size)
        |> Random.Extra.sequence


shuffleDeck : ShuffleKey -> Deck -> Deck
shuffleDeck keylist deck =
    pickCardsUsingKeyList keylist 0 deck


pickCardsUsingKeyList : List Int -> Int -> Deck -> Deck
pickCardsUsingKeyList keylist offset cards =
    case keylist of
        [] ->
            []

        key :: keysRest ->
            let
                ( card, shortList ) =
                    removeAnyCard (offset + key) cards
            in
            case card of
                Nothing ->
                    cards

                Just c ->
                    c :: pickCardsUsingKeyList keysRest (offset + key) shortList


flipCards : Facing -> Deck -> Deck
flipCards facing cards =
    List.map (\c -> flipCard facing c) cards



repeatedly : Int -> (a -> a) -> a -> a
repeatedly count fn x =
    case count of
        0 ->
            x
        n ->
            repeatedly (n - 1) fn (fn x)


times : Int -> (a -> a) -> a -> a
times count fn x =
    case count of
        0 ->
            x
        n ->
            times (n - 1) fn (fn x)


dealToAll : (Maybe Card -> a -> a) -> List a -> Deck -> (List a, Deck)
dealToAll add targets deck  = 
    case (deck, targets) of
        (_, []) -> (targets, deck)
        ([], _) -> (targets, deck)
        (c::drest, t::trest) -> 
            dealToAll add  trest drest |> 
                \(tdone, ddone) -> ((add (Just c) t)::tdone, ddone) 


deal : (Maybe Card -> a -> a) -> a -> Deck -> (a, Deck)
deal add target deck = 
    ((add (List.head deck) target), 
        List.tail deck |> Maybe.withDefault(emptyDeck)) 

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
