module AdventOfCode2022.Code.Day9

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

type Direction = Left | Right | Up | Down
with static member Parse c = match c with 'L' -> Left | 'R' -> Right | 'U' -> Up | 'D' -> Down | c -> failwith $"Invalid input: %c{c}"

type Movement = { Direction : Direction; Steps : int }

let private moveHead (x,y) = function Left -> (x-1,y) | Right -> (x+1,y) | Up -> (x,y-1) | Down -> (x,y+1)

let private moveTail (tx,ty) (hx,hy) =
    let dx = hx - tx
    let dy = hy - ty
    let move = abs dx > 1 || abs dy > 1
    (tx + if move then sign dx else 0), (ty + if move then sign dy else 0)

let a() =
    let move (h,t,visited) d =
        let h = moveHead h d
        let t = moveTail t h
        (h,t,t::visited)
    Day9
    |> getPerLine (fun line ->
        let direction = Direction.Parse line[0]
        Array.init (int (line.Substring 2)) (fun _ -> direction))
    |> Array.collect id
    |> Array.fold move ((0,0),(0,0),[(0,0)])
    |> fun (_,_,v) -> v
    |> List.distinct
    |> List.length

let b() =
    let move (knots,visited) d =
        let knots = List.foldBack (fun cur l -> match l with [] -> [moveHead cur d] | x::xs -> (moveTail cur x)::x::xs) knots []
        (knots,(knots |> List.head)::visited)
    Day9
    |> getPerLine (fun line ->
        let direction = Direction.Parse line[0]
        Array.init (int (line.Substring 2)) (fun _ -> direction))
    |> Array.collect id
    |> Array.fold move ((List.init 10 (fun _ -> (0,0))),[(0,0)])
    |> snd
    |> List.distinct
    |> List.length