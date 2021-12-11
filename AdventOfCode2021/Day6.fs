module AdventOfCode2021.Code.Day6

open Checked
open AdventOfCodeCommon
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let getInput() = Day6 |> split "," |> Seq.map int |> Seq.toList

let processor =
    let processor self daysLeft =
        if daysLeft < 9 then 1L
        else 1L + ((Seq.init ((daysLeft - 9)/7 + 1) (fun n -> self (daysLeft - 9 - 7*n))) |> Seq.sum)
    Utils.memoizeRec processor

let calculate days v = processor (days + 8 - v)

let a() = getInput() |> Seq.map (calculate 80) |> Seq.sum

let b() = getInput() |> Seq.map (calculate 256) |> Seq.sum