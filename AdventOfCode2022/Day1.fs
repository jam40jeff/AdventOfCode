module AdventOfCode2022.Code.Day1

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

let a() =
    Day1
    |> splitOnBlankLines
    |> Seq.map (getIntPerLine >> Seq.sum)
    |> Seq.max

let b() =
    Day1
    |> splitOnBlankLines
    |> Seq.map (getIntPerLine >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum