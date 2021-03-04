-----------------------
-- Gabriela Truta
-- 28.10.2020
-----------------------
module Solutions exposing (..)
import List exposing (sort)

-- deck : List Card
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card Face Suit

deck: List Card
deck =
    let
        faces = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
        suits = [Clubs, Diamonds, Hearts, Spades]
        deckHelper f s =
            List.concatMap (\x -> List.map ( \y -> (Card y x) ) f) s
    in
        deckHelper faces suits

cardValue : Card -> List Int
cardValue (Card face _) =
    case face of
        Ace -> [1, 11]
        Two -> [2]
        Three -> [3]
        Four -> [4]
        Five -> [5]
        Six -> [6]
        Seven -> [7]
        Eight -> [8]
        Nine -> [9]
        Ten -> [10]
        Jack -> [10]
        Queen -> [10]
        King -> [10]

smallestK: Int -> List Int -> List Int
smallestK k list = List.take k (sort list)

balanced : String -> Bool
balanced s =
    let
        list = String.toList s
        balancedHelper right left str =
            case str of
                [] -> if (left == right) then True else False
                x::xs ->
                    case x of
                        '(' -> if (right < left) then False else balancedHelper (right + 1) left xs
                        ')' -> if (right < left) then False else balancedHelper right (left + 1) xs
                        _ -> balancedHelper right left xs
    in
        balancedHelper 0 0 list

coinChange : Int -> List Int -> Int
coinChange sum coins =
    if (sum == 0) then 1
    else if (sum < 0 || List.length coins == 0 ) then 0
    else
        case coins of
            [] -> 0
            x::xs -> coinChange sum xs + coinChange (sum - x) coins