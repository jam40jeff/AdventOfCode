module AdventOfCode2021.Code.Day13

open System
open System.Diagnostics
open Checked
open AdventOfCodeCommon.InputUtils
open AdventOfCodeCommon.Utils
open type AdventOfCodeInput.Input2021

type Fold = AlongX of int | AlongY of int
    with
        static member Parse (s : string) =
            let prefixX = "fold along x="
            let prefixY = "fold along y="
            if s.StartsWith prefixX then AlongX (s |> substringToEnd prefixX.Length |> int)
            else if s.StartsWith prefixY then AlongY (s |> substringToEnd prefixY.Length |> int)
            else failwith $"Invalid fold: %s{s}"

let init() =
    let inputs = Day13 |> split $"%s{newLine}%s{newLine}"
    let points,folds =
        match inputs with
        | [|points;folds|] ->
            let getPoint (s : string) =
                let point = s |> split ","
                match point with
                | [|x;y|] -> (int x,int y)
                | _ -> failwith $"Invalid point: %s{s}"
            let pointsToSet = points |> getPerLine getPoint
            let maxX,_ = pointsToSet |> Seq.maxBy fst
            let _,maxY = pointsToSet |> Seq.maxBy snd
            let points = Array2D.create (maxX + 1) (maxY + 1) false
            pointsToSet |> Seq.iter (fun (x,y) -> points[x,y] <- true)
            points,folds |> getPerLine Fold.Parse
        | _ -> failwith $"Invalid input: %s{Day13}"
    points,folds

let performFold points fold =
    let width = points |> Array2D.length1
    let height = points |> Array2D.length2
    let getPointSafe x y = if x >= width || y >= height then false else points[x,y]
    match fold with
    | AlongX xFold -> Array2D.init xFold height (fun x y -> getPointSafe x y || getPointSafe (2*xFold - x) y)
    | AlongY yFold -> Array2D.init width yFold (fun x y -> getPointSafe x y || getPointSafe x (2*yFold - y))

let a() =
    let points,folds = init()
    performFold points folds[0] |> flattenArray2D |> Seq.filter id |> Seq.length

let b() =
    let points,folds = init()
    let points = folds |> Seq.fold performFold points
    [0..(points |> Array2D.length2) - 1]
        |> Seq.map (fun y ->
            newLine +
                ([0..(points |> Array2D.length1) - 1]
                |> Seq.map (fun x -> if points[x,y] then "\u2588\u2588" else "  ")
                |> String.concat ""))
        |> String.concat ""