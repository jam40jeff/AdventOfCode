module AdventOfCode2023.Code.Day1

open System
open System.Text.RegularExpressions
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2023

let a() =
    Day1
    |> getPerLine (fun s ->
        let digits = s |> Seq.filter Char.IsAsciiDigit |> Seq.map string |> Seq.map Int32.Parse |> Seq.toList
        digits |> List.tryHead |> Option.bind (fun first -> digits |> List.tryLast |> Option.map (fun last -> first*10 + last)) |> Option.defaultValue 0)
    |> Seq.sum

let b() =
    let regexString = "1|one|2|two|3|three|4|four|5|five|6|six|7|seven|8|eight|9|nine"
    let getNum = function
        | "1" | "one" -> 1
        | "2" | "two" -> 2
        | "3" | "three" -> 3
        | "4" | "four" -> 4
        | "5" | "five" -> 5
        | "6" | "six" -> 6
        | "7" | "seven" -> 7
        | "8" | "eight" -> 8
        | "9" | "nine" -> 9
        | s -> failwith $"Unknown number string encountered: %s{s}"
    Day1
    |> getPerLine (fun s ->
        let match1 = Regex.Match(s, regexString)
        let match2 = Regex.Match(s, regexString, RegexOptions.RightToLeft)
        if match1.Success && match2.Success then (getNum match1.Groups[0].Value) * 10 + (getNum match2.Groups[0].Value) else 0)
    |> Seq.sum