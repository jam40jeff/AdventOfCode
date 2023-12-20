module AdventOfCode2023.Code.Day7

open System
open AdventOfCodeCommon
open Checked
open AdventOfCodeCommon.InputUtils
open AdventOfCodeCommon.Utils
open type AdventOfCodeCommon.Utils.Ordering
open type AdventOfCodeInput.Input2023

type Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

let parseCard = function
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eight
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    | c -> failwith $"Unknown Card: %c{c}"

type Score = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind

type Hand =
    {
        Card1 : Card
        Card2 : Card
        Card3 : Card
        Card4 : Card
        Card5 : Card
        Score : Score
    }
    with
        static member Create scoreHand card1 card2 card3 card4 card5 =
            {
                Card1 = card1
                Card2 = card2
                Card3 = card3
                Card4 = card4
                Card5 = card5
                Score = scoreHand [card1;card2;card3;card4;card5]
            }
        member this.AllCards = [this.Card1;this.Card2;this.Card3;this.Card4;this.Card5]

type HandAndBid =
    {
        Hand : Hand
        Bid : int
    }

let a() =
    let scoreHand (cards: Card list) =
        let groupSizes = cards |> Seq.groupBy (fun c -> c) |> Seq.map (snd >> Seq.length) |> Seq.sort |> Seq.toList
        match groupSizes with
        | [5] -> FiveOfAKind
        | [1;4] -> FourOfAKind
        | [2;3] -> FullHouse
        | [1;1;3] -> ThreeOfAKind
        | [1;2;2] -> TwoPair
        | [1;1;1;2] -> OnePair
        | [1;1;1;1;1] -> HighCard
        | _ -> failwith $"Unexpected groups: %A{groupSizes}"
    
    let compareHands x y =
        let scoreComparison = compare x.Score y.Score |> convertComparison
        match scoreComparison with
        | Equal ->
            List.zip x.AllCards y.AllCards
            |> Seq.map (fun (x,y) -> compare x y |> convertComparison)
            |> Seq.filter (fun o -> o <> Equal)
            |> Seq.tryHead
            |> Option.defaultValue Equal
        | Greater -> Greater
        | Less -> Less
        
    Day7
    |> getPerLine (fun s ->
        match s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | [hand;bid] ->
            match bid |> tryParseInt with
            | Some bid ->
                match hand |> Seq.map parseCard |> Seq.toList with
                | [card1;card2;card3;card4;card5] ->
                    {
                        Hand = Hand.Create scoreHand card1 card2 card3 card4 card5
                        Bid = bid
                    }
                | _ -> failwith "Expected hand: %s{hand}"
            | None -> failwith "Expected bid: %s{bid}"
        | _ -> failwith "Unexpected input: %s{s}")
    |> Seq.sortWith (fun x y -> compareHands x.Hand y.Hand |> convertOrdering)
    |> Seq.mapi (fun n handAndBid -> handAndBid.Bid*(n + 1))
    |> Seq.sum

let b() =
    let scoreHand (cards: Card list) =
        let groupSizes = cards |> Seq.groupBy (fun c -> c) |> Seq.map (snd >> Seq.length) |> Seq.sort |> Seq.toList
        let numJacks = cards |> Seq.filter (fun c -> c = Jack) |> Seq.length
        match groupSizes with
        | [5] -> FiveOfAKind
        | [1;4] -> if numJacks > 0 then FiveOfAKind else FourOfAKind
        | [2;3] -> if numJacks > 0 then FiveOfAKind else FullHouse
        | [1;1;3] -> if numJacks > 0 then FourOfAKind else ThreeOfAKind
        | [1;2;2] -> if numJacks = 2 then FourOfAKind else if numJacks = 1 then FullHouse else TwoPair
        | [1;1;1;2] -> if numJacks > 0 then ThreeOfAKind else OnePair
        | [1;1;1;1;1] -> if numJacks > 0 then OnePair else HighCard
        | _ -> failwith $"Unexpected groups: %A{groupSizes}"
    
    let compareHands x y =
        let scoreComparison = compare x.Score y.Score |> convertComparison
        match scoreComparison with
        | Equal ->
            List.zip x.AllCards y.AllCards
            |> Seq.map (fun (x,y) ->
                match (x,y) with
                | Jack,Jack -> Equal
                | Jack,_ -> Less
                | _,Jack -> Greater
                | _ -> compare x y |> convertComparison)
            |> Seq.filter (fun o -> o <> Equal)
            |> Seq.tryHead
            |> Option.defaultValue Equal
        | Greater -> Greater
        | Less -> Less
        
    Day7
    |> getPerLine (fun s ->
        match s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | [hand;bid] ->
            match bid |> tryParseInt with
            | Some bid ->
                match hand |> Seq.map parseCard |> Seq.toList with
                | [card1;card2;card3;card4;card5] ->
                    {
                        Hand = Hand.Create scoreHand card1 card2 card3 card4 card5
                        Bid = bid
                    }
                | _ -> failwith "Expected hand: %s{hand}"
            | None -> failwith "Expected bid: %s{bid}"
        | _ -> failwith "Unexpected input: %s{s}")
    |> Seq.sortWith (fun x y -> compareHands x.Hand y.Hand |> convertOrdering)
    |> Seq.mapi (fun n handAndBid -> handAndBid.Bid*(n + 1))
    |> Seq.sum