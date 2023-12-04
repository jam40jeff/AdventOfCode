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

let flattenArray2Di a = 
    seq {
        for x in [0..(Array2D.length1 a) - 1] do 
            for y in [0..(Array2D.length2 a) - 1] do 
                yield (x,y,a[x,y])
    }

let groupTuplesAndMap f t = t |> Seq.groupBy fst |> Seq.map (fun (key,values) -> key,values |> Seq.map snd |> f)
let groupTuples t = groupTuplesAndMap Seq.toList t

let private tryMinOrMaxBy c f s =
    s
    |> Seq.fold
        (fun prev cur ->
            let curC = f cur
            match prev with
            | None -> Some (cur,curC)
            | Some (prev,prevC) -> Some (if c curC prevC then (cur,curC) else (prev,prevC)))
        None
    |> Option.map fst

let tryMinBy f s = s |> tryMinOrMaxBy (<) f
let tryMin s = s |> tryMinBy id
let tryMaxBy f s = s |> tryMinOrMaxBy (>) f
let tryMax s = s |> tryMaxBy id

let takeWhileInclusive f s =
    seq { yield (true, Unchecked.defaultof<'a>); yield! (s |> Seq.map (fun v -> (f v, v))) }
    |> Seq.pairwise
    |> Seq.takeWhile (fst >> fst)
    |> Seq.map (snd >> snd)

let rec gcd (x : int64) (y : int64) =
    match (x,y) with
    | x,y when x = y -> x
    | x,y when x > y -> gcd (x-y) y
    | x,y -> gcd x (y-x)

let gcdl x =
    match x with
    | [] -> 0L
    | a::_ -> x |> List.fold gcd a