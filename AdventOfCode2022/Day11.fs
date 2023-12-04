module AdventOfCode2022.Code.Day11

open System.Collections.Generic
open AdventOfCodeCommon
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

type Monkey =
    {
        mutable Inspections: int64
        Items : Queue<int64>
        Operation : int64 -> int64
        TestDivisor : int64
        OnTrue : int
        OnFalse : int
    }

let parseInput() =
    Day11
    |> splitOnBlankLines
    |> Seq.map (fun group ->
        let lines = group |> splitLines
        let items = lines[1].Trim().Substring("Starting items: ".Length).Split(',') |> Seq.map (fun s -> s.Trim() |> int64)
        let operationString = lines[2].Trim().Substring("Operation: new = old ".Length)
        let operation =
            match operationString.Substring(2) with
            | "old" ->
                match operationString[0] with
                | '*' -> (fun x -> x * x)
                | '+' -> (fun x -> x + x)
                | c -> failwith $"Invalid operator: %c{c}"
            | s ->
                let v = int64 s
                match operationString[0] with
                | '*' -> (fun x -> x * v)
                | '+' -> (fun x -> x + v)
                | c -> failwith $"Invalid operator: %c{c}"
        let testDivisor = lines[3].Trim().Substring("Test: divisible by ".Length) |> int64
        let onTrue = lines[4].Trim().Substring("If true: throw to monkey ".Length) |> int
        let onFalse = lines[5].Trim().Substring("If false: throw to monkey ".Length) |> int
        {
            Inspections = 0
            Items = Queue<_>(items)
            Operation = operation
            TestDivisor = testDivisor
            OnTrue = onTrue
            OnFalse = onFalse
        })
    |> Seq.toList

let a() =
    let monkeys = parseInput()
    [1..20]
    |> Seq.iter (fun _ ->
        monkeys |> Seq.iter (fun monkey ->
            while monkey.Items.Count > 0 do
                let item = (monkey.Items.Dequeue() |> monkey.Operation) / 3L
                monkey.Inspections <- monkey.Inspections + 1L
                let newMonkey =
                    if item % monkey.TestDivisor = 0 then monkeys[monkey.OnTrue] else monkeys[monkey.OnFalse]
                newMonkey.Items.Enqueue item))
    monkeys |> Seq.map (fun monkey -> monkey.Inspections) |> Seq.sortDescending |> Seq.take 2 |> Seq.fold (*) 1L

let b() =
    let monkeys = parseInput()
    let divisor = monkeys |> Seq.map (fun monkey -> monkey.TestDivisor) |> Seq.fold (*) 1L
    [1..10000]
    |> Seq.iter (fun n ->
        monkeys |> Seq.iter (fun monkey ->
            while monkey.Items.Count > 0 do
                let item = monkey.Items.Dequeue() |> monkey.Operation
                monkey.Inspections <- monkey.Inspections + 1L
                let newMonkey =
                    if item % monkey.TestDivisor = 0 then monkeys[monkey.OnTrue] else monkeys[monkey.OnFalse]
                newMonkey.Items.Enqueue item)
        monkeys |> Seq.iter (fun monkey ->
            let items = monkey.Items |> Seq.toList
            monkey.Items.Clear()
            items |> Seq.map (fun item -> item % divisor) |> Seq.iter monkey.Items.Enqueue))
    monkeys |> Seq.map (fun monkey -> monkey.Inspections) |> Seq.sortDescending |> Seq.take 2 |> Seq.fold (*) 1L