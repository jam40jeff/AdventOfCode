module AdventOfCode2021.Code.Day2

open Checked
open System.Text.RegularExpressions
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

module private Input =
    type Direction = Forward | Down | Up
    type T = { Direction : Direction; Distance : int }
        with
            member this.Dx = match this.Direction with Forward -> this.Distance | _ -> 0
            member this.Dy = (match this.Direction with Up -> -1 | Down -> 1 | _ -> 0) * this.Distance
    let private regex = Regex("^(forward|down|up) ([0-9]+)$")
    let parse line =
        let matches = regex.Match line
        if not matches.Success then failwith $"Input \"{line}\" did not match expected format."
        {
            Direction = match matches.Groups[1].Value with "forward" -> Forward | "down" -> Down | "up" -> Up | v -> failwithf $"Unexpected direction: %s{v}"
            Distance = matches.Groups[2].Value |> int
        }

let a() =
    let x, y =
        Day2
        |> getPerLine Input.parse
        |> Seq.fold (fun (prevX, prevY) cur -> prevX + cur.Dx, prevY + cur.Dy) (0, 0)
    x * y

let b() =
    let x, y, _ =
        Day2
        |> getPerLine Input.parse
        |> Seq.fold (fun (prevX, prevY, aim) cur -> prevX + cur.Dx, prevY + aim * cur.Dx, aim + cur.Dy) (0, 0, 0)
    x * y