module AdventOfCode2021.Code.Day5

open Checked
open AdventOfCodeCommon
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let run includeDiagonals =
    let getPoints s =
        let points = s |> split " -> " |> Array.map (fun p -> p |> split "," |> Array.map int)
        match points with
        | [|[|x1;y1|];[|x2;y2|]|] -> (x1,y1),(x2,y2)
        | _ -> failwith $"Invalid format for line: %s{l}"
    
    let vents = Day5 |> getPerLine getPoints
    
    let values =
        let maxX = vents |> Seq.map (fun ((x1,_),(x2,_)) -> max x1 x2) |> Seq.max
        let maxY = vents |> Seq.map (fun ((_,y1),(_,y2)) -> max y1 y2) |> Seq.max
        Array2D.create (maxX + 1) (maxY + 1) 0
    
    let getPointsAlongLine ((x1,y1),(x2,y2)) =
        let getStep v1 v2 = if v2 >= v1 then 1 else -1
        if x1 = x2 then [y1..(getStep y1 y2)..y2] |> Seq.map (fun y -> (x1,y))
        else if y1 = y2 then [x1..(getStep x1 x2)..x2] |> Seq.map (fun x -> (x,y1))
        else if includeDiagonals then Seq.zip [x1..(getStep x1 x2)..x2] [y1..(getStep y1 y2)..y2]
        else Seq.empty

    let increment (x,y) = values[x,y] <- values[x,y] + 1
    vents |> Seq.iter (getPointsAlongLine >> Seq.iter increment)
    
    let isOverlap v = v > 1
    values |> Utils.flattenArray2D |> Seq.filter isOverlap |> Seq.length

let a() = run false

let b() = run true