module AdventOfCode2021.Code.Day12

open System
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

type Cave = Start | Small of string | Large of string | End
    with
        static member Parse s =
            match s with
            | "start" -> Start
            | "end" -> End
            | s -> if Char.IsLower s[0] then Small s else Large s

let parseLine (line : string) =
    let caves = line.Split "-"
    match caves with
    | [|caveFrom;caveTo|] ->
        let caveFrom = Cave.Parse caveFrom
        let caveTo = Cave.Parse caveTo
        seq { (caveFrom,caveTo);(caveTo,caveFrom) }
    | _ -> failwith $"Invalid input: %s{line}"

let notStart = function Start -> false | _ -> true
let groupTuples t = t |> Seq.groupBy fst |> Seq.map (fun (key,values) -> key,values |> Seq.map snd |> Seq.toList)

let a() =
    let caves = Day12 |> getPerLine parseLine |> Seq.collect id |> Seq.distinct |> groupTuples |> Map.ofSeq
    let rec visitCave smallCavesVisited cave =
        match cave with
        | Small cave when smallCavesVisited |> Set.contains cave -> 0
        | End -> 1
        | cave ->
            match caves |> Map.tryFind cave with
            | None -> 0
            | Some newCaves ->
                let smallCavesVisited = match cave with Small cave -> smallCavesVisited |> Set.add cave | _ -> smallCavesVisited
                newCaves |> Seq.filter notStart |> Seq.sumBy (visitCave smallCavesVisited)
    visitCave Set.empty Start

let b() =
    let caves = Day12 |> getPerLine parseLine |> Seq.collect id |> Seq.distinct |> groupTuples |> Map.ofSeq
    let rec visitCave smallCavesVisited hasVisitedAnySmallCaveTwice cave =
        match cave with
        | Small cave when hasVisitedAnySmallCaveTwice && smallCavesVisited |> Set.contains cave -> 0
        | End -> 1
        | cave ->
            match caves |> Map.tryFind cave with
            | None -> 0
            | Some newCaves ->
                let smallCavesVisited,hasVisitedAnySmallCaveTwice =
                    match cave with
                    | Small cave ->
                        let hasVisitedAnySmallCaveTwice = hasVisitedAnySmallCaveTwice || smallCavesVisited |> Set.contains cave
                        smallCavesVisited |> Set.add cave,hasVisitedAnySmallCaveTwice
                    | _ -> smallCavesVisited,hasVisitedAnySmallCaveTwice
                newCaves |> Seq.filter notStart |> Seq.sumBy (visitCave smallCavesVisited hasVisitedAnySmallCaveTwice)
    visitCave Set.empty false Start