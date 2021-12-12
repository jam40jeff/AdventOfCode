module AdventOfCode2021.Code.Day5

open Checked
open AdventOfCodeCommon
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let getStep v1 v2 = if v2 >= v1 then 1 else -1

let getLinePoints ((x1,y1),(x2,y2)) =
    if x1 = x2 then
        [y1..(getStep y1 y2)..y2] |> Seq.map (fun y -> (x1,y))
    else if y1 = y2 then
        [x1..(getStep x1 x2)..x2] |> Seq.map (fun x -> (x,y1))
    else Seq.empty

let getLinePointsWithDiagonals ((x1,y1),(x2,y2)) =
    if x1 = x2 || y1 = y2 then getLinePoints ((x1,y1),(x2,y2))
    else Seq.zip [x1..(getStep x1 x2)..x2] [y1..(getStep y1 y2)..y2]

let run getLinePoints =
    let increment (values : _[,]) (x,y) = values[x,y] <- values[x,y] + 1

    let countOverlaps values =
        let isOverlap v = v > 1
        values |> Utils.flattenArray2D |> Seq.filter isOverlap |> Seq.length
    
    let vents =
        Day5
        |> getPerLine (fun l ->
            let points = l |> split " -> " |> Array.map (fun p -> p |> split "," |> Array.map int)
            match points with
            | [|[|x1;y1|];[|x2;y2|]|] -> (x1,y1),(x2,y2)
            | _ -> failwith $"Invalid format for line: %s{l}")
    
    let values =
        let maxX = vents |> Seq.map (fun ((x1,_),(x2,_)) -> max x1 x2) |> Seq.max
        let maxY = vents |> Seq.map (fun ((_,y1),(_,y2)) -> max y1 y2) |> Seq.max
        Array2D.create (maxX + 1) (maxY + 1) 0
    
    vents |> Seq.iter (getLinePoints >> Seq.iter (increment values))
    values |> countOverlaps

let a() = run getLinePoints

let b() = run getLinePointsWithDiagonals