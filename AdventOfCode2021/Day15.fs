module AdventOfCode2021.Code.Day15

open System
open System.Collections.Generic
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let run input =
    let width = input |> Array2D.length1
    let height = input |> Array2D.length2
    let maxX = width - 1
    let maxY = height - 1
    let scores = Array2D.create width height Int32.MaxValue
    scores[0,0] <- 0
    
    let rec scan() =
        for n = 1 to (max maxX maxY)*2 + 1 do
            for x = 0 to n do
                let y = n - x
                if x >= 0 && y >= 0 && x <= maxX && y <= maxY then
                    let value = input[x,y] + if x > 0 then (if y > 0 then min scores[x-1,y] scores[x,y-1] else scores[x-1,y]) else scores[x,y-1]
                    if value < scores[x,y] then scores[x,y] <- value
        
        let upMoves = List<(int*int)*int>()
        for x = 0 to maxX do
            for y = 1 to maxY do
                let diff = scores[x,y] + input[x,y-1] - scores[x,y-1]
                if diff < 0 then upMoves.Add((x,y-1),diff)
        
        let leftMoves = List<(int*int)*int>()
        if upMoves.Count < 1 then
            for x = 1 to maxX do
                for y = 0 to maxY do
                    let diff = scores[x,y] + input[x-1,y] - scores[x-1,y]
                    if diff < 0 then upMoves.Add((x-1,y),diff)
        
        if upMoves.Count > 0 || leftMoves.Count > 0 then
            
            for (x,y),diff in upMoves do scores[x,y] <- scores[x,y] + diff
            for (x,y),diff in leftMoves do scores[x,y] <- scores[x,y] + diff
            
            scan()
    
    scan()
    
    scores[maxX,maxY]

let a() = Day15 |> getPerLine (Seq.map (Char.GetNumericValue >> int)) |> array2D |> run

let b() =
    let input = Day15 |> getPerLine (Seq.map (Char.GetNumericValue >> int)) |> array2D
    let width = input |> Array2D.length1
    let height = input |> Array2D.length2
    let input = Array2D.init (5*width) (5*height) (fun x y ->
        let x' = x % width
        let y' = y % height
        let addX = x / width
        let addY = y / width
        (input[x',y'] + addX + addY - 1) % 9 + 1)
    input |> run