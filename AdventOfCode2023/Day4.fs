module AdventOfCode2023.Code.Day4

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2023

let private getNums (s : string) = s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse

let private getMatches (s : string) =
    let s = (s.Split(": ", StringSplitOptions.RemoveEmptyEntries)[1]).Split(" | ", StringSplitOptions.RemoveEmptyEntries)
    let winners = getNums s[0] |> Set.ofSeq
    getNums s[1] |> Seq.filter winners.Contains |> Seq.length

let a() =
    let processRow line =
        let numMatches = getMatches line
        if numMatches < 1 then 0 else pown 2 (numMatches - 1)
    
    Day4 |> getPerLine processRow |> Seq.sum

let b() =
    let cards = Dictionary<int,int>()

    let processRow row line =
        let increment row n =
            let found,numCards = cards.TryGetValue row
            cards[row] <- if found then numCards + n else n
        
        let numMatches = getMatches line
        increment row 1
        let n = cards[row]
        for i in 1..numMatches do increment (row + i) n
    
    Day4 |> getStringPerLine |> Seq.iteri processRow
    
    cards.Values |> Seq.sum