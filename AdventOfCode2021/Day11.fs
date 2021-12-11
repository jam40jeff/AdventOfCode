module AdventOfCode2021.Code.Day11

open System
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021

let init() =
    let grid =
        Day11
        |> getPerLine (Seq.map (fun c -> c |> Char.GetNumericValue |> int,false))
        |> array2D
    
    let maxX = (grid |> Array2D.length1) - 1
    let maxY = (grid |> Array2D.length2) - 1
    
    let step() = grid |> Array2D.iteri (fun x y (n,flashed) -> grid[x,y] <- (if flashed then 1 else n + 1),false)
    
    let rec scan x y flashes =
        let applyFlash x y =
            [-1..1] |> Seq.iter (fun dx -> [-1..1] |> Seq.iter (fun dy ->
                if dx <> 0 || dy <> 0 then
                    let x = x + dx
                    let y = y + dy
                    if x >= 0 && y >= 0 && x <= maxX && y <= maxY then
                        let n,flashed = grid[x,y]
                        grid[x,y] <- n + 1,flashed))
        
        let x,y = if x > maxX then 0,y + 1 else x,y
        if y > maxY then flashes
        else
            let n,flashed = grid[x,y]
            if n > 9 && not flashed then
                grid[x,y] <- n,true
                applyFlash x y
                scan 0 0 (flashes + 1)
            else scan (x + 1) y flashes
    
    step,scan

let a() =
    let step,scan = init()
    [1..100] |> Seq.sumBy (fun _ -> step(); scan 0 0 0)

let b() =
    let step,scan = init()
    
    Seq.initInfinite id
    |> Seq.choose
        (fun n ->
            let flashed = scan 0 0 0
            if flashed = 100 then Some n
            else
                step()
                None)
    |> Seq.head