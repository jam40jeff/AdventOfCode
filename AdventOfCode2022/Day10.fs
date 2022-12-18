module AdventOfCode2022.Code.Day10

open System
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

type private Operation = Noop | AddX of int
with static member Parse = function
        | "noop" -> Noop
        | s when s.Substring(0,4) = "addx" ->
            if s.Length < 6 then failwith $"Invalid input: %s{s}" else AddX (s.Substring 5 |> int)
        | s -> failwith $"Invalid input: %s{s}"

let a() =
    Day10
    |> getPerLine Operation.Parse
    |> Seq.fold
            (fun (x,n,vals) o ->
                match o with
                | Noop -> (x,n+1,if (n + 20)%40 = 0 then x*n::vals else vals)
                | AddX dx -> (x+dx,n+2,if (n + 20)%40 = 0 then x*n::vals else if (n + 21)%40 = 0 then x*(n+1)::vals else vals))
            (1,1,[])
    |> fun (_,_,v) -> v
    |> List.sum

let b() =
    let pos =
        Day10
        |> getPerLine Operation.Parse
        |> Seq.fold
                (fun (x,n,vals) o ->
                    let check n =
                        let n = (n - 1)%40
                        n - x >= -1 && n - x <= 1
                    match o with
                    | Noop -> (x,n+1,if check n then n::vals else vals)
                    | AddX dx -> (x+dx,n+2,(if check (n+1) then [n+1] else [])@(if check n then n::vals else vals)))
                (1,1,[])
        |> fun (_,_,v) -> v
    let s = List.init 240 (fun n -> if pos |> List.contains n then '#' else '.') |> List.chunkBySize 40 |> List.map (fun chars -> String(chars |> Seq.toArray))
    Environment.NewLine + String.Join(Environment.NewLine, s)