module AdventOfCode2021.Code.Day17

open System.Text.RegularExpressions
open Checked
open type AdventOfCodeInput.Input2021

let regex = Regex("""target area\: x=(-?[0-9]+)\.\.(-?[0-9]+), y=(-?[0-9]+)\.\.(-?[0-9]+)""")

let getInput() =
    let m = Day17 |> regex.Match
    let x1 = int m.Groups[1].Value
    let x2 = int m.Groups[2].Value
    let y1 = int m.Groups[3].Value
    let y2 = int m.Groups[4].Value
    min x1 x2,max x1 x2,min y1 y2,max y1 y2

let a() =
    let _,_,minY,maxY = getInput()
    let yv = if minY < 0 then -1*minY - 1 else maxY
    [1..yv] |> List.sum

let b() =
    let minX,maxX,minY,maxY = getInput()
    
    let minXV = if minX < 0 then minX else 0
    let maxXV = if maxX > 0 then maxX else 0
    
    let xCandidates =
        [minXV..maxXV]
        |> Seq.filter (fun xv ->
            if xv = 0 then true
            else
                let step = if xv < 0 then 1 else -1
                [xv..step..0]
                |> Seq.scan
                    (fun (prevX,_) curXV ->
                        if prevX >= minX && prevX <= maxX then prevX,true
                        else prevX + curXV,false)
                    (0,false)
                |> Seq.exists snd)
        |> Seq.toList
    
    let minYV = if minY > 0 then 0 else minY
    let maxYV = if minY < 0 then -1*minY - 1 else maxY
    let yCandidates = [minYV..maxYV]
    
    xCandidates
    |> Seq.collect
        (fun xv ->
            let xDir = -1*(sign xv)
            yCandidates
            |> Seq.filter (fun yv ->
                Seq.initInfinite (fun n -> xv + xDir*n,yv - n)
                |> Seq.scan
                    (fun (prevX,prevY,_) (xv,yv) ->
                        let xv = if sign xv = sign xDir then 0 else xv
                        if (prevY < minY && yv < 0) || (prevX > maxX && prevX > 0) || (prevX < minX && prevX < 0) then 0,0,Some false
                        else if prevX >= minX && prevX <= maxX && prevY >= minY && prevY <= maxY then 0,0,Some true
                        else prevX + xv,prevY + yv,None)
                    (0,0,None)
                |> Seq.choose (fun (_,_,result) -> result)
                |> Seq.head))
    |> Seq.length