module AdventOfCodeCommon.Utils

open Checked
open System.Collections.Concurrent

let rec combinations size set =
    let rec combinations acc size set = seq {
        match size, set with 
        | n, x::xs ->
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs 
        | 0, [] -> yield acc 
        | _, [] -> () }
    combinations [] size set

let rec permutations l =
    let rec insertions x = function
    | [] -> [[x]]
    | y::ys as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))
    match l with
    | [] -> Seq.singleton []
    | x::xs -> Seq.collect (insertions x) (permutations xs)

let memoizeRec f =
    let cache = ConcurrentDictionary<_,_>()
    let rec f' x =
        let found, y = cache.TryGetValue x
        if found then y
        else cache.AddOrUpdate(x, (fun x -> f f' x), (fun x y -> y))
    f'

let memoize f = memoizeRec (fun _ -> f)

let flattenArray2D a = 
    seq {
        for x in [0..(Array2D.length1 a) - 1] do 
            for y in [0..(Array2D.length2 a) - 1] do 
                yield a[x,y]
    }

let groupTuplesAndMap f t = t |> Seq.groupBy fst |> Seq.map (fun (key,values) -> key,values |> Seq.map snd |> f)
let groupTuples t = groupTuplesAndMap Seq.toList t