module AdventOfCode2021.Code.Day5

open AdventOfCodeCommon
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let init() =
    let vents =
        Day5
        |> getPerLine (fun l ->
            let points = l |> split " -> " |> Array.map (fun p -> p |> split "," |> Array.map int)
            match points with
            | [|[|x1;y1|];[|x2;y2|]|] -> (x1,y1),(x2,y2)
            | _ -> failwith $"Invalid format for line: %s{l}")
    let maxX = vents |> Seq.map (fun ((x1,_),(x2,_)) -> max x1 x2) |> Seq.max
    let maxY = vents |> Seq.map (fun ((_,y1),(_,y2)) -> max y1 y2) |> Seq.max
    let values = Array2D.create (maxX + 1) (maxY + 1) 0
    vents, values

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

let a() =
    let vents, values = init()
    vents |> Seq.iter (getLinePoints >> Seq.iter (fun (x,y) -> values[x,y] <- values[x,y] + 1))
    values |> Utils.flattenArray2D |> Seq.filter (fun v -> v > 1) |> Seq.length

let b() =
    let vents, values = init()
    vents |> Seq.iter (getLinePointsWithDiagonals >> Seq.iter (fun (x,y) -> values[x,y] <- values[x,y] + 1))
    values |> Utils.flattenArray2D |> Seq.filter (fun v -> v > 1) |> Seq.length