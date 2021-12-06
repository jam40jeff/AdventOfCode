module AdventOfCode2021.Code.Day6

open AdventOfCodeCommon
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let getInput() = Day6 |> split "," |> Seq.map int |> Seq.toList

let rec processor self daysLeft =
    if daysLeft < 9 then 1L
    else 1L + ((Seq.init ((daysLeft - 9)/7 + 1) (fun n -> self (daysLeft - 9 - 7*n))) |> Seq.sum)

let calculate processor days v = processor (days + 8 - v)

let a() =
    let processor = Utils.memoizeRec processor
    getInput() |> Seq.map (calculate processor 80) |> Seq.sum

let b() =
    let processor = Utils.memoizeRec processor
    getInput() |> Seq.map (calculate processor 256) |> Seq.sum