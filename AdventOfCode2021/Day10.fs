module AdventOfCode2021.Code.Day10

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let isOpen c = c = '(' || c = '[' || c = '{' || c = '<'
let isClose c = c = ')' || c = ']' || c = '}' || c = '>'

let isIllegal stack c =
    let isMatch o c =
        c =
            match o with
            | '(' -> ')'
            | '[' -> ']'
            | '{' -> '}'
            | '<' -> '>'
            | o -> failwith $"Unexpected open character: %c{o}"

    match stack with [] -> true,[] | x::xs -> isMatch x c |> not,xs

let a() =
    let getIllegalChar c =
            c
            |> Seq.scan
                (fun (stack,_) c ->
                    match c with
                    | c when isClose c ->
                        let isIllegal,stack = isIllegal stack c
                        if isIllegal then [],Some c else stack,None
                    | c when isOpen c -> ((c::stack),None)
                    | c -> failwith $"Unexpected character: %c{c}")
                ([],None)
            |> Seq.choose snd
            |> Seq.tryHead

    let getValue c =
        match c with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | c -> failwith $"Unexpected illegal character: %c{c}"
    
    Day10
    |> getPerLine (Seq.map id)
    |> Seq.map getIllegalChar
    |> Seq.choose id
    |> Seq.sumBy getValue

let b() =
    let getRemainingStack c =
        let rec getRemainingStack stack c =
            match c with
            | [] -> Some stack
            | x::xs ->
                match x with
                | x when isClose x ->
                    let isIllegal,stack = isIllegal stack x
                    if isIllegal then None else getRemainingStack stack xs
                | x when isOpen x -> getRemainingStack (x::stack) xs
                | x -> failwith $"Unexpected character: %c{x}"
        c |> Seq.toList |> getRemainingStack []
    
    let getValue c =
        match c with
        | '(' -> 1L
        | '[' -> 2L
        | '{' -> 3L
        | '<' -> 4L
        | c -> failwith $"Unexpected illegal character: %c{c}"
    
    let scores =
        Day10
        |> getPerLine (Seq.map id)
        |> Seq.map getRemainingStack 
        |> Seq.choose id
        |> Seq.map (Seq.map getValue >> Seq.reduce ((*) 5L >> (+)))
        |> Seq.sort
        |> Seq.toList
    
    scores[scores.Length/2]