module AdventOfCode2021.Code.Day8

open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let nums = [
    Set.ofList ['a';'b';'c';'e';'f';'g']
    Set.ofList ['c';'f']
    Set.ofList ['a';'c';'d';'e';'g']
    Set.ofList ['a';'c';'d';'f';'g']
    Set.ofList ['b';'c';'d';'f']
    Set.ofList ['a';'b';'d';'f';'g']
    Set.ofList ['a';'b';'d';'e';'f';'g']
    Set.ofList ['a';'c';'f']
    Set.ofList ['a';'b';'c';'d';'e';'f';'g']
    Set.ofList ['a';'b';'c';'d';'f';'g']
]

type UniqueDigits = UniqueDigits of char Set list
    with
        static member Parse s = s |> split " " |> Seq.map Set.ofSeq |> Seq.toList |> UniqueDigits
        member this.Values = match this with UniqueDigits u -> u
        member this.getMapping() =
            let values = this.Values
            let one = values |> Seq.filter (Set.count >> (=) 2) |> Seq.exactlyOne
            let four = values |> Seq.filter (Set.count >> (=) 4) |> Seq.exactlyOne
            let seven = values |> Seq.filter (Set.count >> (=) 3) |> Seq.exactlyOne
            let eight = values |> Seq.filter (Set.count >> (=) 7) |> Seq.exactlyOne
            let six = values |> Seq.filter (fun c -> c.Count = 6 && one |> Set.exists (fun v -> c |> Set.contains v |> not)) |> Seq.exactlyOne
            let nine = values |> Seq.filter (fun c -> c.Count = 6 && four |> Set.forall (fun v -> c |> Set.contains v)) |> Seq.exactlyOne
            let zero = values |> Seq.filter (fun c -> c.Count = 6 && c <> six && c <> nine) |> Seq.exactlyOne
            let five = values |> Seq.filter (fun c -> c.Count = 5 && c |> Set.forall (fun v -> six |> Set.contains v)) |> Seq.exactlyOne
            let three = values |> Seq.filter (fun c -> c.Count = 5 && seven |> Set.forall (fun v -> c |> Set.contains v)) |> Seq.exactlyOne
            let two = values |> Seq.filter (fun c -> c <> zero && c <> one && c <> three && c <> four && c <> five && c <> six && c <> seven && c <> eight && c <> nine) |> Seq.exactlyOne
            let a = seven |> Seq.filter (fun c -> one |> Set.contains c |> not) |> Seq.exactlyOne
            let e = two |> Seq.filter (fun c -> three |> Set.contains c |> not) |> Seq.exactlyOne
            let f = three |> Seq.filter (fun c -> two |> Set.contains c |> not) |> Seq.exactlyOne
            let d = eight |> Seq.filter (fun c -> zero |> Set.contains c |> not) |> Seq.exactlyOne
            let b = nine |> Seq.filter (fun c -> three |> Set.contains c |> not) |> Seq.exactlyOne
            let c = one |> Seq.filter (fun c -> five |> Set.contains c |> not) |> Seq.exactlyOne
            let g = nine |> Seq.filter (fun c -> c <> a && four |> Set.contains c |> not) |> Seq.exactlyOne
            Map.ofList [
                Set.ofList [a;b;c;e;f;g], 0
                Set.ofList [c;f], 1
                Set.ofList [a;c;d;e;g], 2
                Set.ofList [a;c;d;f;g], 3
                Set.ofList [b;c;d;f], 4
                Set.ofList [a;b;d;f;g], 5
                Set.ofList [a;b;d;e;f;g], 6
                Set.ofList [a;c;f], 7
                Set.ofList [a;b;c;d;e;f;g], 8
                Set.ofList [a;b;c;d;f;g], 9
            ]

type OutputDigits = OutputDigits of char Set list
    with
        static member Parse s = s |> split " " |> Seq.map Set.ofSeq |> Seq.toList |> OutputDigits
        member this.Values = match this with OutputDigits o -> o

let getInput() =
    Day8
    |> getPerLine
        (fun s ->
            let values = s |> split " | "
            match values with
            | [|v1;v2|] -> UniqueDigits.Parse v1, OutputDigits.Parse v2
            | _ -> failwith $"Invalid input: %s{s}")

let a() =
    let is1or4or7or8 (c : char Set) =
        let stringLengths = [2;3;4;7]
        stringLengths |> List.contains c.Count
    
    getInput() |> Seq.map (fun (_, outputDigits) -> outputDigits.Values |> Seq.filter is1or4or7or8 |> Seq.length) |> Seq.sum

let b() =
    getInput()
    |> Seq.map
        (fun (uniqueDigits, outputDigits) ->
            let mapping = uniqueDigits.getMapping()
            let outputDigitValues = outputDigits.Values
            let len = outputDigitValues.Length
            outputDigitValues |> Seq.mapi (fun i v -> pown 10 (len - i - 1) * (mapping |> Map.find v)) |> Seq.sum)
    |> Seq.sum