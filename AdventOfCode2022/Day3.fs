module AdventOfCode2022.Code.Day3

open Checked
open System
open AdventOfCodeCommon.InputUtils

open type AdventOfCodeInput.Input2022

let private priority (c: char) = if Char.IsLower c then (int c) - (int 'a') + 1 else (int c) - (int 'A') + 27

let a () =
    Day3
    |> getPerLine (fun s ->
        let mid = s.Length / 2
        let items1 = s.Substring(0, mid) |> Set.ofSeq
        let items2 = s.Substring mid |> Set.ofSeq
        let item = items1 |> Set.intersect items2 |> Set.toSeq |> Seq.exactlyOne
        priority item)
    |> Seq.sum

let b () =
    let aggregateSets p c = match p with | None -> Some c | Some p -> p |> Set.intersect c |> Some

    Day3
    |> getStringPerLine
    |> Seq.chunkBySize 3
    |> Seq.map (
        Seq.map Set.ofSeq
        >> Seq.fold aggregateSets None
        >> Option.bind (Set.toSeq >> Seq.tryExactlyOne)
        >> Option.defaultWith (fun () -> failwith "Could not find common item in group.")
        >> priority)
    |> Seq.sum