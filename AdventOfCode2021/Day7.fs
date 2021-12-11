module AdventOfCode2021.Code.Day7

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let getInput() = Day7 |> split "," |> Seq.map int |> Seq.toArray

let a() =
    let sortedInput = getInput() |> Array.sort
    let midpoint = (sortedInput.Length - 1 |> float)/2.0
    let valueLow = sortedInput[midpoint |> floor |> int]
    let valueHigh = sortedInput[midpoint |> ceil |> int]
    let pos = seq { valueLow; valueHigh } |> Seq.maxBy (fun v -> sortedInput |> Seq.filter ((=) v) |> Seq.length)
    sortedInput |> Seq.map ((-) pos >> abs) |> Seq.sum

let b() =
    let input = getInput()
    
    let calc pos =
        input
        |> Seq.map
            (fun n ->
                let d = pos - n |> abs
                (d + 1 |> float) * (float d / 2.0) |> int)
        |> Seq.sum
    
    let pos = input |> Seq.map float |> Seq.average |> round |> int
    let min, v = seq { pos - 1; pos; pos + 1 } |> Seq.mapi (fun i pos -> (i, calc pos)) |> Seq.minBy snd
    let dir = sign (min - 1)
    if dir = 0 then
        v
    else
        let rec findMin v i =
            let newValue = calc i
            if newValue >= v then v
            else findMin newValue (i + dir)
        findMin v pos