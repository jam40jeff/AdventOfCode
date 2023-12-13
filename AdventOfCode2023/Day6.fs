module AdventOfCode2023.Code.Day6

open System
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2023

type private TimeAndDistance = { Time : int64; Distance : int64 }

let a() =
    let timesAndDistances =
        let lines =
            Day6
            |> getPerLine (fun s ->
                match s.Split(":") |> Array.toList with
                | _::[s] -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int64.Parse |> Seq.toList
                | _ -> failwith $"Unexpected input: %s{s}")
             |> Array.toList
        match lines with
        | times::[distances] -> Seq.zip times distances |> Seq.map (fun (time,distance) -> { Time = time; Distance = distance }) |> Seq.toList
        | _ -> failwith $"Unexpected input: %s{Day6}"
    
    let getWinnerCount timeAndDistance =
        [0L..timeAndDistance.Time] |> Seq.filter (fun x -> x*(timeAndDistance.Time - x) > timeAndDistance.Distance) |> Seq.length |> int64
        
    timesAndDistances |> Seq.map getWinnerCount |> Seq.fold (*) 1L

let b() =
    let timeAndDistance =
        let lines =
            Day6
            |> getPerLine (fun s ->
                match s.Split(":") |> Array.toList with
                | _::[s] -> String.Join("", s.Split(" ", StringSplitOptions.RemoveEmptyEntries)) |> Int64.Parse
                | _ -> failwith $"Unexpected input: %s{s}")
             |> Array.toList
        match lines with
        | time::[distance] -> { Time = time; Distance = distance }
        | _ -> failwith $"Unexpected input: %s{Day6}"
    
    let getWinnerCount timeAndDistance =
        let start = (timeAndDistance.Time |> float)/2.0 |> floor |> int64
        let test x = x*(timeAndDistance.Time - x) > timeAndDistance.Distance
        (Seq.initInfinite (fun n -> start + (n |> int64) + 1L) |> Seq.takeWhile test |> Seq.length) +
        (Seq.initInfinite (fun n -> start - (n |> int64)) |> Seq.takeWhile test |> Seq.length)
        
    getWinnerCount timeAndDistance