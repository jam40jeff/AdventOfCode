module AdventOfCode2021.Code.Day1

open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let a() =
    Day1
    |> getIntPerLine
    |> Seq.fold
        (fun (prev, count) curr ->
            let isGreater = match prev with | None -> false | Some prev -> curr > prev
            Some curr, count + if isGreater then 1 else 0)
        (None, 0)
    |> snd

let a'() =
    Day1
    |> getIntPerLine
    |> Seq.pairwise
    |> Seq.filter (fun (x, y) -> y > x)
    |> Seq.length

let b() =
    let _, _, _, count =
        Day1
        |> getIntPerLine
        |> Seq.fold
            (fun (prev3, prev2, prev1, count) curr ->
                let isGreater = match prev3 with | None -> false | Some prev3 -> curr > prev3
                prev2, prev1, Some curr, count + if isGreater then 1 else 0)
            (None, None, None, 0)
    count

let b'() =
    Day1
    |> getIntPerLine
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> Seq.pairwise
    |> Seq.filter (fun (x, y) -> y > x)
    |> Seq.length