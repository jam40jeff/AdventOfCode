module AdventOfCode2021.Code.Day9

open Checked
open System
open AdventOfCodeCommon
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let init() =
    let input = Day9 |> getPerLine (Seq.map (Char.GetNumericValue >> int)) |> array2D
    let xMax = (input |> Array2D.length1) - 1
    let yMax = (input |> Array2D.length2) - 1
    
    let getAdjacentIndices x y =
        [(x - 1,y);(x + 1,y);(x,y - 1);(x,y + 1)]
        |> Seq.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= xMax && y <= yMax)

    let isMin x y v = getAdjacentIndices x y |> Seq.forall (fun (x,y) -> v < input[x,y])
    
    input,isMin,getAdjacentIndices

let a() =
    let input,isMin,_ = init()
    
    input
    |> Array2D.mapi (fun x y v -> v,isMin x y v)
    |> Utils.flattenArray2D
    |> Seq.filter snd
    |> Seq.sumBy (fst >> (+) 1)

let b() =
    let input,isMin,getAdjacentIndices = init()
    
    let minPoints =
        input
        |> Array2D.mapi (fun x y v -> (x,y),isMin x y v)
        |> Utils.flattenArray2D
        |> Seq.filter snd
        |> Seq.map fst
        |> Seq.toList
    
    let getBasinSize (x,y) =
        let rec getBasinSize x y size seen =
            if input[x,y] = 9 || seen |> Set.contains (x,y) then size,seen
            else
                getAdjacentIndices x y
                |> Seq.fold
                    (fun (size,seen) (x,y) -> getBasinSize x y size seen)
                    (size + 1,seen |> Set.add (x,y))
        let size,_ = getBasinSize x y 0 Set.empty
        size
    
    minPoints
    |> Seq.map getBasinSize
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)