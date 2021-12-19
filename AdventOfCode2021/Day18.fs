module AdventOfCode2021.Code.Day18

open System
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

type Value = { mutable Value : int64 }
type Number = Pair of Number*Number | Value of Value

let rec private numberToString = function
    | Pair (left,right) -> $"[%s{numberToString left},%s{numberToString right}]"
    | Value v -> v.Value.ToString()

let private getInput() =
    let rec parseNumber s =
        let toString c = c |> List.toArray |> String
        let rec parseNumber s =
            match s with
            | '['::s ->
                let left,s = parseNumber s
                match s with
                | ','::s ->
                    let right,s = parseNumber s
                    let pair = Pair (left,right)
                    match s with
                    | ']'::s -> pair,s
                    | s -> failwith $"Expected closing bracket at: %s{toString s}"
                | s -> failwith $"Expected comma at: %s{toString s}"
            | s ->
                let rec getNumber n s =
                    if s |> List.length < 1 then (List.rev n),s
                    else
                        let c = s[0]
                        if Char.IsNumber c then getNumber (c::n) (List.tail s) else (List.rev n),s
                let n,s = getNumber [] s
                let n = if n |> List.isEmpty then failwith $"Expected number at: %s{toString s}" else n |> toString |> int64
                Value { Value = n },s
        let n,s = parseNumber (s |> Seq.map id |> Seq.toList)
        if s |> List.isEmpty |> not then failwith $"Non-empty list at end of parse: %s{toString s}"
        n
    
    Day18 |> getPerLine parseNumber

let private add n1 n2 = Pair (n1,n2)

let private explode n =
    let rec explode n depth lastValue add hasExploded =
        match n with
        | Value left,Value right ->
            if not hasExploded && depth >= 4 then
                match lastValue with None -> () | Some v -> v.Value <- v.Value + left.Value
                Value { Value = 0 },None,right.Value,true
            else
                let rightValue = { Value = right.Value }
                Pair (Value { Value = left.Value + add },Value rightValue),Some rightValue,0,hasExploded
        | Value left,Pair (left',right') ->
            let leftValue = { Value = left.Value + add }
            let left = Value leftValue
            let right,lastValue,add,hasExploded = explode (left',right') (depth + 1) (Some leftValue) 0L hasExploded
            Pair (left,right),lastValue,add,hasExploded
        | Pair (left',right'),Value right ->
            let left,_,add,hasExploded = explode (left',right') (depth + 1) lastValue add hasExploded
            let rightValue = { Value = right.Value + add }
            let right = Value rightValue
            Pair (left,right),Some rightValue,0,hasExploded
        | Pair (left',right'),Pair (left'',right'') ->
            let left,lastValue,add,hasExploded = explode (left',right') (depth + 1) lastValue add hasExploded
            let right,lastValue,add,hasExploded = explode (left'',right'') (depth + 1) lastValue add hasExploded
            Pair (left,right),lastValue,add,hasExploded
    let n,_,_,hasExploded =
        match n with
        | Pair (left,right) -> explode (left,right) 0 None 0 false
        | Value _ -> failwith "Can only explode a top level Pair."
    n,hasExploded

let rec private split n =
    let rec split n hasSplit =
        match n with
        | Pair (left,right) ->
            let left,hasSplit = split left hasSplit
            let right,hasSplit = split right hasSplit
            Pair (left,right),hasSplit
        | Value v ->
            if hasSplit || v.Value < 10 then Value v,hasSplit
            else
                let left = v.Value/2L
                let right = v.Value - left
                Pair (Value { Value = left },Value { Value = right }),true
    split n false

let rec private getMagnitude = function
    | Pair (left,right) -> 3L*(getMagnitude left) + 2L*(getMagnitude right)
    | Value v -> v.Value

let rec private reduce n =
    let n,hasExploded = explode n
    let n,hasExplodedOrSplit =
        if not hasExploded then split n else n,hasExploded
    if not hasExplodedOrSplit then n else reduce n

let a() =
    let reduceAll n = n |> Seq.reduce (fun p c -> add p c |> reduce)
    getInput() |> reduceAll |> getMagnitude

let b() =
    let input = getInput()
    
    input
    |> Seq.mapi (fun i x -> input |> Seq.mapi (fun j y -> if i = j then None else Some (x,y)))
    |> Seq.collect id
    |> Seq.choose id
    |> Seq.map (fun (x,y) -> add x y |> reduce |> getMagnitude)
    |> Seq.max