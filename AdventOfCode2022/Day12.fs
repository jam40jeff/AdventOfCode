module AdventOfCode2022.Code.Day12

open System.Collections.Generic
open AdventOfCodeCommon
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

let parseInput() =
    let grid = Day12 |> getPerLine (fun line -> seq { yield! line }) |> array2D
    let start = grid |> Utils.flattenArray2Di |> Seq.choose (fun (x,y,v) -> if v = 'S' then Some (x,y) else None) |> Seq.exactlyOne
    grid,(grid |> Array2D.length1) - 1,(grid |> Array2D.length2) - 1,start

let a() =
    let grid,maxX,maxY,start = parseInput()
    let total = (maxX+1)*(maxY+1)
    let visitedFromStart = Dictionary<_,_>()
    let visitedToEnd = Dictionary<_,_>()
    let mutable minVisitedToEnd = None
    let mutable iterations = 0
    let rec visit (x,y) n path =
        iterations <- iterations + 1
        if visitedFromStart.Count % 10 = 0 then printfn $"Visited %i{visitedFromStart.Count} of %i{total}."
        if iterations % 100 = 0 then printfn $"%i{iterations} of %i{total*4} total iterations."
        let thisPoint = x,y
        if path |> List.contains thisPoint then None
        else if minVisitedToEnd |> Option.map (fun minVisitedToEnd -> n >= minVisitedToEnd) |> Option.defaultValue false then None
        else if x < 0 || x > maxX || y < 0 || y > maxY then None
        else if visitedToEnd.ContainsKey (x,y) then visitedToEnd[(x,y)]
        else if visitedFromStart.ContainsKey (x,y) && visitedFromStart[(x,y)] <= n then None
        else
            visitedFromStart[(x,y)] <- n
            let v = grid[x,y]
            if v = 'E' then Some 1
            else
                let results =
                    [(1,0);(0,1);(-1,0);(0,-1)]
                    |> Seq.map (fun (dx,dy) -> (x+dx,y+dy))
                    |> Seq.map (fun (x,y) -> if x < 0 || x > maxX || y < 0 || y > maxY || (v <> 'S' && (int grid[x,y]) - (int v) > 1) then None else visit (x,y) (n+1) (thisPoint::path))
                    |> Seq.choose id
                    |> Seq.toList
                match results with
                | [] -> None
                | l ->
                    let result = l |> Seq.min
                    visitedToEnd.Add((x,y),Some result)
                    minVisitedToEnd <-
                        Some
                            (match minVisitedToEnd with
                             | Some minVisitedToEnd -> if result < minVisitedToEnd then minVisitedToEnd else result
                             | None -> result)
                    Some result
    visit start 0 [] |> Option.defaultWith (fun () -> failwith "No solution found.")

let b() =
    Day1
    |> splitOnBlankLines
    |> Seq.map (getIntPerLine >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum