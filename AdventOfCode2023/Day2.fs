module AdventOfCode2023.Code.Day2

open System
open System.Text.RegularExpressions
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2023

let private regex = Regex(" ([0-9]+) (red|green|blue)")

type Color = Red | Green | Blue

type Cubes =
    {
        Num : int
        Color : Color
    }

let getCubes(m : Match) =
    {
        Num = Int32.Parse m.Groups[1].Value
        Color = match m.Groups[2].Value with "red" -> Red | "green" -> Green | "blue" -> Blue | s -> failwithf $"Unknown input: %s{s}"
    }

let a() =
    let isInvalid cubes =
        match cubes.Color with
            | Red -> cubes.Num > 12
            | Green -> cubes.Num > 13
            | Blue -> cubes.Num > 14
    
    let getGame(s : string) = s.Split(" ") |> Array.last |> Int32.Parse
    
    let gameNumIfPossible(s : string) =
        let gameAndCubes = s.Split(": ")
        let gameString = gameAndCubes |> Array.head
        let cubesString = gameAndCubes |> Array.last
        if regex.Matches cubesString |> Seq.map getCubes |> Seq.exists isInvalid then None else Some (getGame gameString)
    
    Day2
    |> getPerLine gameNumIfPossible
    |> Seq.choose id
    |> Seq.sum

let b() =
    let getPower(s : string) =
        regex.Matches s
        |> Seq.map getCubes
        |> Seq.groupBy (fun cube -> cube.Color)
        |> Seq.map (fun (_, g) -> g |> Seq.maxBy (fun cube -> cube.Num))
        |> Seq.fold (fun n c -> n * c.Num) 1
    
    Day2
    |> getPerLine getPower
    |> Seq.sum