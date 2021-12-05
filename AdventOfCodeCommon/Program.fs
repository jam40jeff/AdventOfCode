namespace AdventOfCodeCommon

open System
open System.Diagnostics

type Day = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23 | D24 | D25
type Part = A | B

module Program =
    let execute map =
        
        let rec getDay() =
            printf "Enter day to solve (1-25): "
            let day = Console.ReadLine()
            let valid, day = Int32.TryParse day
            let day = if valid then Some day else None
            match day with
            | Some 1 -> D1
            | Some 2 -> D2
            | Some 3 -> D3
            | Some 4 -> D4
            | Some 5 -> D5
            | Some 6 -> D6
            | Some 7 -> D7
            | Some 8 -> D8
            | Some 9 -> D9
            | Some 10 -> D10
            | Some 11 -> D11
            | Some 12 -> D12
            | Some 13 -> D13
            | Some 14 -> D14
            | Some 15 -> D15
            | Some 16 -> D16
            | Some 17 -> D17
            | Some 18 -> D18
            | Some 19 -> D19
            | Some 20 -> D20
            | Some 21 -> D21
            | Some 22 -> D22
            | Some 23 -> D23
            | Some 24 -> D24
            | Some 25 -> D25
            | _ ->
                printfn "A valid integer between 1 and 25 must be entered."
                getDay()
        
        let day = getDay()
        
        let rec getPart() =
            printf "Enter part to solve (a or b): "
            let part = Console.ReadLine()
            match part with
            | part when part = "a" || part = "A" -> A
            | part when part = "b" || part = "B" -> B
            | _ ->
                printfn "Either a or b must be entered."
                getPart()
        
        let part = getPart()
        
        match map |> Map.tryFind (day, part) with
        | Some f ->
            let stopwatch = Stopwatch()
            stopwatch.Start()
            let s = f()
            stopwatch.Stop()
            printfn $"Solution is: %s{s}"
            printfn $"Solved in %d{stopwatch.ElapsedMilliseconds}ms."
        | None -> printfn "No solution yet."