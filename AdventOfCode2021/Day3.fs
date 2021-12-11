module AdventOfCode2021.Code.Day3

open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let private bitsToInt (bits : bool[]) =
    bits
    |> Seq.mapi (fun i bit -> if bit then 1 <<< (bits.Length - 1 - i) else 0)
    |> Seq.sum

let private is1 i (v : string) = v[i] = '1'
let private boolToInt b = if b then 1 else 0
let private boolToChar b = if b then '1' else '0'
let private isMajority1 len num1 = num1 >= len - num1
let private isMinority1 len num1 = num1 < len - num1

let a() =
    let input = Day3 |> getStringPerLine
    let sLen = input[0].Length
    let getRate bitCriteria =
        [|0..sLen - 1|]
        |> Array.map (fun i -> input |> Seq.sumBy (is1 i >> boolToInt) |> bitCriteria input.Length)
        |> bitsToInt
    (getRate isMajority1) * (getRate isMinority1)

let b() =
    let input = Day3 |> getStringPerLine
    let getRating bitCriteria =
        let rating =
            [0..input[0].Length - 1]
            |> Seq.fold
                (fun matchingItems i ->
                    match matchingItems with
                    | [] -> failwith "All items filtered out."
                    | [x] -> [x]
                    | matchingItems ->
                        let num1 = matchingItems |> Seq.filter (is1 i) |> Seq.length
                        let c = bitCriteria matchingItems.Length num1 |> boolToChar
                        matchingItems |> List.filter (fun v -> v[i] = c))
                (input |> Array.toList)
        match rating with
        | [] -> failwith "All items filtered out."
        | [x] -> x |> Seq.map (fun c -> c = '1') |> Seq.toArray |> bitsToInt
        | _ -> failwith "More than one item found."
    (getRating isMajority1) * (getRating isMinority1)