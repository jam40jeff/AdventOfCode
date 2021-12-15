module AdventOfCode2021.Code.Day14

open Checked
open AdventOfCodeCommon.InputUtils
open AdventOfCodeCommon.Utils
open type AdventOfCodeInput.Input2021

let init() =
    let groups = Day14 |> split $"%s{newLine}%s{newLine}" |> Seq.map getStringPerLine |> Seq.toList
    let template,rules =
        match groups with
        | [[|template|];rules] -> template,rules
        | _ -> failwith $"Invalid input: %s{Day14}"
    let getRule s =
        match s |> split " -> " |> Seq.map (Seq.map id >> Seq.toList) |> Seq.toList with
        | [[letterBefore;letterAfter];[insertionLetter]] -> ((letterBefore,letterAfter),insertionLetter)
        | _ -> failwith $"Invalid input rule: %s{s}"
    template,rules |> Seq.map getRule |> Map.ofSeq

let a() =
    let template,rules = init()
    
    let executeStep s _ =
        match s |> Seq.toList with
        | firstChar::rest ->
            rest
            |> Seq.fold
                (fun (prev,result) cur ->
                    cur,result + (match rules |> Map.tryFind (prev,cur) with None -> "" | Some v -> string v) + string cur)
                (firstChar,string firstChar)
            |> snd
        | _ -> failwith $"Invalid template: {s}"
    
    let result =
        [1..10]
        |> Seq.fold executeStep template
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.sort
        |> Seq.toList
    
    (result |> List.last) - (result |> List.head)

let b() =
    let template,rules = init()
    let rules = rules |> Map.toSeq |> Seq.map (fun ((c1,c2),c) -> (c1,c2),[(c1,c);(c,c2)]) |> Map.ofSeq
    let initialCounts = template |> Seq.pairwise |> Seq.countBy id |> Seq.map (fun (k,v) -> (k,int64 v)) |> Map.ofSeq
    
    let executeStep (counts : Map<_,int64>) _ =
        counts
        |> Map.toSeq
        |> Seq.collect (fun (p,c) -> rules |> Map.tryFind p |> Option.defaultValue List.empty |> Seq.map (fun p -> (p,c)))
        |> groupTuplesAndMap Seq.sum
        |> Map.ofSeq
    
    let result =
        [1..40]
        |> Seq.fold executeStep initialCounts
        |> Map.toSeq
        |> Seq.collect (fun ((c1,c2),n) -> seq { (c1,n); (c2,n) })
        |> groupTuplesAndMap Seq.sum
        |> Seq.map snd
        |> Seq.map (fun n -> (n + 1L)/2L)
        |> Seq.sort
        |> Seq.toList
    
    (result |> List.last) - (result |> List.head)