module AdventOfCode2022.Code.Day4

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

type Range = { Min : int; Max : int }

let parseLine (line : string) =
    let parseRange (s : string) =
        let parts = s.Split('-')
        { Min = int parts[0]; Max = int parts[1] }
    let parts = line.Split(',')
    (parseRange parts[0],parseRange parts[1])

let a() =
    let isEitherContainedWithin (range1,range2) =
        let isContainedWithin range1 range2 = range1.Min >= range2.Min && range1.Max <= range2.Max
        isContainedWithin range1 range2 || isContainedWithin range2 range1
    Day4
    |> getPerLine parseLine
    |> Seq.filter isEitherContainedWithin
    |> Seq.length

let b() =
    let isOverlapping (range1,range2) = range1.Min <= range2.Max && range1.Max >= range2.Min
    Day4
    |> getPerLine parseLine
    |> Seq.filter isOverlapping
    |> Seq.length