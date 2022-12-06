module AdventOfCode2022.Code.Day6

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

let calc numDistinct =
    (Day6
    |> Seq.windowed numDistinct
    |> Seq.takeWhile (fun c -> c |> Seq.distinct |> Seq.length < numDistinct)
    |> Seq.length) + numDistinct

let a() = calc 4

let b() = calc 14