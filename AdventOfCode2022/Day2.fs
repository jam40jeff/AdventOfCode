module AdventOfCode2022.Code.Day2

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

type RockPaperScissors = Rock | Paper | Scissors

let private points other mine =
    let playedPoints = function Rock -> 1 | Paper -> 2 | Scissors -> 3
    let winnerPoints other = function
        | Rock -> match other with Rock -> 3 | Paper -> 0 | Scissors -> 6
        | Paper -> match other with Rock -> 6 | Paper -> 3 | Scissors -> 0
        | Scissors -> match other with Rock -> 0 | Paper -> 6 | Scissors -> 3
    playedPoints mine + winnerPoints other mine

let private readOther = function 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | s -> failwith $"Invalid input: {s}"

let a() =
    let readMine = function 'X' -> Rock | 'Y' -> Paper | 'Z' -> Scissors | s -> failwith $"Invalid input: {s}"
    Day2
    |> getPerLine (fun line ->
        let other = readOther line[0]
        let mine = readMine line[2]
        points other mine)
    |> Seq.sum

let b() =
    let readMine other = function
        | 'X' -> match other with Rock -> Scissors | Paper -> Rock | Scissors -> Paper
        | 'Y' -> match other with Rock -> Rock | Paper -> Paper | Scissors -> Scissors
        | 'Z' -> match other with Rock -> Paper | Paper -> Scissors | Scissors -> Rock
        | s -> failwith $"Invalid input: {s}"
    Day2
    |> getPerLine (fun line ->
        let other = readOther line[0]
        let mine = readMine other line[2]
        points other mine)
    |> Seq.sum