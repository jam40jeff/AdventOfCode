module AdventOfCode2023.Code.Day3

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2023

type private Pos = { Row : int; Col : int }
type private Num = { Pos : Pos; Value : int; Length : int }

let private getSymbolsAndNums input symbolTest =
    let symbols = List<_>()
    let nums = List<_>()
    
    let handleRow row line =
        let mutable currentNum = None
        line
        |> Seq.iteri (fun col c ->
            if Char.IsAsciiDigit c then
                let digit = c.ToString() |> Int32.Parse
                currentNum <-
                    Some
                        (match currentNum with
                        | Some currentNum -> { currentNum with Value = currentNum.Value*10 + digit; Length = currentNum.Length + 1 }
                        | None -> { Pos = { Row = row; Col = col }; Value = digit; Length = 1 })
            else
                if symbolTest c then symbols.Add { Row = row; Col = col }
                currentNum |> Option.iter nums.Add
                currentNum <- None)
        currentNum |> Option.iter nums.Add
    
    input |> getStringPerLine |> Seq.iteri handleRow
    
    (symbols |> Seq.toList, nums |> Seq.toList)

let private areAdjacent symbol num =
    abs (symbol.Row - num.Pos.Row) <= 1 && num.Pos.Col - symbol.Col <= 1 && symbol.Col - num.Pos.Col <= num.Length

let a() =
    let symbols,nums = getSymbolsAndNums Day3 (fun c -> c <> '.')
    let isAdjacentToSymbol num = symbols |> Seq.exists (fun symbol -> areAdjacent symbol num)
    nums |> Seq.filter isAdjacentToSymbol |> Seq.map (fun num -> num.Value) |> Seq.sum

let b() =
    let stars,nums = getSymbolsAndNums Day3 (fun c -> c = '*')
    
    let gearRatio star =
        let nums = nums |> Seq.filter (fun num -> areAdjacent star num) |> Seq.map (fun num -> num.Value) |> Seq.toList
        match nums with
        | [n1;n2] -> n1*n2 |> Some
        | _ -> None
    
    stars |> Seq.choose gearRatio |> Seq.sum