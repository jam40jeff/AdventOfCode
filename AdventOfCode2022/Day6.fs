module AdventOfCode2022.Code.Day6

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

let a() =
    (Day6
    |> Seq.windowed 4
    |> Seq.takeWhile (fun c -> c |> Seq.distinct |> Seq.length < 4)
    |> Seq.length) + 4

let b() =
    (Day6
    |> Seq.windowed 14
    |> Seq.takeWhile (fun c -> c |> Seq.distinct |> Seq.length < 14)
    |> Seq.length) + 14