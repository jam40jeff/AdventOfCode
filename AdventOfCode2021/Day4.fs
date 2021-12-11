module AdventOfCode2021.Code.Day4

open Checked
open System
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let getBoards input =
    input
    |> Seq.skip 1
    |> Seq.chunkBySize 6
    |> Seq.filter (Array.length >> (=) 6)
    |> Seq.map (Seq.skip 1 >> Seq.map (splitWithOptions " " StringSplitOptions.RemoveEmptyEntries >> Seq.map int))
    |> Seq.map array2D
    |> Seq.toList

let isWinner numbersChosen (board : int[,]) =
    if numbersChosen |> Set.count < 5 then false
    else
        [0..4] |> Seq.exists (fun i ->
            [0..4] |> Seq.forall (fun j -> numbersChosen |> Set.contains board[i,j])
                || [0..4] |> Seq.forall (fun j -> numbersChosen |> Set.contains board[j,i]))

let calculateScore numbersChosen n (board : int[,]) =
    ([0..4] |> Seq.collect (fun x -> [0..4] |> Seq.map (fun y -> board[x,y])) |> Seq.filter (fun v -> numbersChosen |> Set.contains v |> not) |> Seq.sum) * n

let a() =
    let input = Day4 |> splitLines
    let numbers = input[0] |> split "," |> Seq.map int
    let boards = input |> getBoards
    
    let sln, _ =
        numbers
        |> Seq.fold
            (fun (sln, numbersChosen) n ->
                let numbersChosen = numbersChosen |> Set.add n
                match sln with
                | Some sln -> Some sln, numbersChosen
                | None -> (boards |> Seq.tryFind (isWinner numbersChosen) |> Option.map (calculateScore numbersChosen n)), numbersChosen)
            (None, Set.empty)
    
    sln |> Option.defaultWith (fun () -> failwith "No solution found.")

let b() =
    let input = Day4 |> splitLines
    let numbers = input[0] |> split "," |> Seq.map int
    let boards = input |> getBoards
    
    let sln, _, _ =
        numbers
        |> Seq.fold
            (fun (sln, numbersChosen, boardsLeft) n ->
                let numbersChosen = numbersChosen |> Set.add n
                match boardsLeft |> Array.filter (isWinner numbersChosen) with
                | [||] -> sln, numbersChosen, boardsLeft
                | winningBoards -> Some (winningBoards |> Array.last |> calculateScore numbersChosen n), numbersChosen, boardsLeft |> Array.except winningBoards)
            (None, Set.empty, boards |> Seq.toArray)
    
    sln |> Option.defaultWith (fun () -> failwith "No solution found.")