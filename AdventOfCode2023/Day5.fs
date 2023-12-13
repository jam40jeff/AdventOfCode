module AdventOfCode2023.Code.Day5

open System
open System.Collections.Generic
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2023

type private MapRange = { DestStart : int64; SourceStart : int64; Length : int64 }

let private getMap (s : string) =
    let getMapRange (line : string) =
        match line.Split(" ") |> Array.toList with
        | dest::src::[n] ->
            let dest = Int64.Parse dest
            let src = Int64.Parse src
            let n = Int64.Parse n
            { DestStart = dest; SourceStart = src; Length = n }
        | _ -> failwith $"Unexpected seed input: %s{line}."
    s |> getStringPerLine |> Seq.skip 1 |> Seq.map getMapRange |> Seq.toList

let a() =
    match Day5 |> splitOnBlankLines |> Array.toList with
    | seeds::blocks ->
        match seeds.Split(": ") |> Array.toList with
        | _::[seeds] ->
            let seeds = seeds.Split(" ") |> Seq.map Int64.Parse |> Seq.toList
            
            let maps = blocks |> Seq.map getMap |> Seq.toList
            
            let mapValue (value : int64) map =
                map
                |> Seq.filter (fun r -> value >= r.SourceStart && value < r.SourceStart + r.Length)
                |> Seq.tryHead
                |> Option.map (fun r -> value - r.SourceStart + r.DestStart)
                |> Option.defaultValue value
            
            seeds |> Seq.map (fun seed -> maps |> Seq.fold mapValue seed) |> Seq.min
        | _ -> failwith $"Unexpected seeds input: %s{seeds}."
    | _ -> failwith $"Unexpected input: %s{Day5}."

type private Range = { Start : int64; Length : int64 }

let b() =
    match Day5 |> splitOnBlankLines |> Array.toList with
    | seeds::blocks ->
        match seeds.Split(": ") |> Array.toList with
        | _::[seeds] ->
            let seeds =
                seeds.Split(" ")
                |> Seq.map Int64.Parse
                |> Seq.chunkBySize 2
                |> Seq.map Seq.toList
                |> Seq.map (fun c ->
                    match c with
                    | start::[length] -> { Start = start; Length = length }
                    | _ -> failwith $"Unexpected seed input: %A{c}")
                |> Seq.toList
            
            let mapValue (value : int64) (length : int64) maps =
                maps
                |> Seq.filter (fun r -> value >= r.SourceStart && value < r.SourceStart + r.Length)
                |> Seq.tryHead
                |> Option.map (fun r -> { Start = value + r.DestStart - r.SourceStart; Length = min (r.Length + r.SourceStart - value) length })
                |> Option.defaultWith (fun () -> maps |> Seq.filter (fun r -> r.SourceStart > value) |> Seq.map (fun r -> { Start = value; Length = min (r.SourceStart - value) length }) |> Seq.tryHead |> Option.defaultValue { Start = value; Length = length })
            
            let mapRange range maps =
                let result = List<_>()
                let mutable value = range.Start
                while value < range.Start + range.Length do
                    let range = mapValue value (range.Length + range.Start - value) maps
                    value <- value + range.Length
                    result.Add range
                result |> Seq.toList
            
            let ranges =
                blocks
                |> Seq.map getMap
                |> Seq.fold (fun prev maps -> prev |> Seq.collect (fun r -> mapRange r maps)) seeds
                |> Seq.toList
            
            ranges |> Seq.map (fun r -> r.Start) |> Seq.min
        | _ -> failwith $"Unexpected seeds input: %s{seeds}."
    | _ -> failwith $"Unexpected input: %s{Day5}."