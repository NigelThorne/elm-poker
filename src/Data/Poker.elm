module Data.Poker exposing (..)

import Data.Cards exposing (..)
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




-}
{-
   Decisions?
   Add players or already in game config? -- add players to game fixed number of possible seats

   Poker
   * Hand of poker has a dealer, a big and little blind
   config  => blinds schedule - what blinds start at - when they go up.
           => number of cards in flop, turn, river
           => number of starting chips
           => name of room

   newGame : PokerConfig -> PokerGame
   startNextHand : PokerGame -> PokerGame
   dealerDeals : PokerGame -> PokerGame
   playerBids : PokerGame -> bid -> PokerGame
   playerFolds : PokerGame -> Player -> PokerGame
   playerDeals : PokerGame -> Player -> PokerGame

-}


type alias Game =
    { deck : Cards
    , community : Cards
    , players : List Player
    , pot : Chips
    }

type alias GameId =
    Int

type alias Player =
    { hand : Cards
    , bank : Chips
    , name : String
    , stake : Int
    }

type alias Chips = 
    Int

noChips : Chips
noChips = 0
noCards : Cards
noCards = []


initGame : Cards -> List String -> Chips -> Game
initGame deck playerNames startingChips =
    { deck = deck
    , community = noCards
    , players = (initPlayers playerNames startingChips)
    , pot = noChips
    }

initPlayers : List String -> Chips -> List Player
initPlayers names startingChips =
    names |> 
        List.map (\name -> initPlayer name startingChips)

initPlayer : String -> Chips -> Player
initPlayer name chips = 
    { name = name
    , hand = noCards
    , bank = chips
    , stake = noChips
    }


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
-- dealACardToEachPlayer : ( List Player, Deck ) -> ( List Player, Deck )
-- dealACardToEachPlayer ( players, fullDeck ) =
--     List.foldl step { players = [], remainingDeck = fullDeck } players
-- step : Player -> {players: List Player, remainingDeck: Deck} -> { players: List Player, remainingDeck: Deck }
-- step player {players, deck} =
--     dealCardToPlayer ( player, deck )
--         |> (\( p, d ) -> ( players ++ [ p ], d ))
-- dealCardToPlayer : ( Player, Deck ) -> ( Player, Deck )
-- dealCardToPlayer ( player, deck ) =
--     Data.Deck.dealCards 1 FaceDown {to = player.cards, from = deck}
--         |> (\( c, d ) -> ( { player | cards = c }, d ))

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






flipHand : Int -> Facing -> List Player -> List Player
flipHand index facing players =
    updateAt index (\p -> { p | hand = (flipCards facing p.hand) }) players


flipCards : Facing -> List Card -> List Card
flipCards facing cards =
    List.map (\c -> flipCard facing c) cards


flop : Game -> Game
flop game =
    times 3 dealCommunityCard game


dealCommunityCard : Game -> Game
dealCommunityCard game =
    let
        ( cards, remains ) =
            deal (addCard FaceUp) game.community game.deck
    in
    { game | deck = remains, community = cards }


turn : Game -> Game
turn game =
    dealCommunityCard game


river : Game -> Game
river game =
    dealCommunityCard game


dealPlayerCards : Game -> Game
dealPlayerCards game =
    ( game.players, game.deck ) |> (dealCardsToEachHand 2) |> \( players, deck ) -> { game | players = players, deck = deck }


dealCardsToEachHand : Int -> ( List Player, Cards ) -> ( List Player, Cards )
dealCardsToEachHand count state =
    times count dealCardToEachHand state


dealCardToEachHand : ( List Player, Cards ) -> ( List Player, Cards )
dealCardToEachHand ( players, deck ) =
    dealToAll addCardToPlayer players deck 


clearTable : Game -> Game
clearTable game = 
    { game | community = noCards , pot = noChips}

addCardToPlayer : Maybe Card -> Player -> Player
addCardToPlayer card player =
    (addCard FaceDown card player.hand) |> 
        \(d) -> {player | hand = d} 



dealCardToHand : ( Player, Cards ) -> ( Player, Cards )
dealCardToHand ( player, deck ) =
    let
        ( h, _ ) =
            deal (addCard FaceDown) player.hand deck
    in
    ( { player | hand = h }, deck )
