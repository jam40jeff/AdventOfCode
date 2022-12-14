module AdventOfCode2022.Code.Day8

open AdventOfCodeCommon
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

let getGrid() =
    let grid = Day8 |> getPerLine (Seq.map int) |> array2D
    grid, (grid |> Array2D.length1) - 1, (grid |> Array2D.length2) - 1

let a() =
    let grid, maxX, maxY = getGrid()
    let isVisible x y value =
        if x = 0 || x = maxX || y = 0 || y = maxY then true
        else
            let isVisibleToLeft() = [x - 1..-1..0] |> Seq.forall (fun x -> grid[x,y] < value)
            let isVisibleToRight() = [x + 1..maxX] |> Seq.forall (fun x -> grid[x,y] < value)
            let isVisibleToTop() = [y - 1..-1..0] |> Seq.forall (fun y -> grid[x,y] < value)
            let isVisibleToBottom() = [y + 1..maxY] |> Seq.forall (fun y -> grid[x,y] < value)
            isVisibleToLeft() || isVisibleToRight() || isVisibleToTop() || isVisibleToBottom()
    grid |> Array2D.mapi isVisible |> Utils.flattenArray2D |> Seq.filter id |> Seq.length

let b() =
    let grid, maxX, maxY = getGrid()
    let scenicScore x y value =
        if x = 0 || x = maxX || y = 0 || y = maxY then 0
        else
            let scenicScoreToLeft() = [x - 1..-1..0] |> Utils.takeWhileInclusive (fun x -> grid[x,y] < value) |> Seq.length
            let scenicScoreToRight() = [x + 1..maxX] |> Utils.takeWhileInclusive (fun x -> grid[x,y] < value) |> Seq.length
            let scenicScoreToTop() = [y - 1..-1..0] |> Utils.takeWhileInclusive (fun y -> grid[x,y] < value) |> Seq.length
            let scenicScoreToBottom() = [y + 1..maxY] |> Utils.takeWhileInclusive (fun y -> grid[x,y] < value) |> Seq.length
            let l = scenicScoreToLeft()
            if l = 0 then 0
            else
                let r = scenicScoreToRight()
                if r = 0 then 0
                else
                    let t = scenicScoreToTop()
                    if t = 0 then 0
                    else
                        let b = scenicScoreToBottom()
                        if b = 0 then 0
                        else l * r * t * b
    grid |> Array2D.mapi scenicScore |> Utils.flattenArray2D |> Seq.max