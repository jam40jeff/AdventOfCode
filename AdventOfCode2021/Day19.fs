module AdventOfCode2021.Code.Day19

open System
open System.IO
open AdventOfCodeCommon
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2021
open MathNet.Numerics.LinearAlgebra

module Point =
    type Transform = private { Matrix : Matrix<double> }
    
    let private matrixT v = { Matrix = matrix v }
    
    //let transformations = [matrixT [[]]]
    let transformations =
        [
            matrixT [[  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0; -1.0;  0.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0;  1.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  1.0;  0.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0; -1.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  1.0;  0.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0;  1.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0; -1.0;  0.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0; -1.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [  1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0; -1.0;  0.0 ]
                     [  0.0; -1.0;  0.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0; -1.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  1.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  0.0;  1.0;  0.0 ]
                     [  0.0;  1.0;  0.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
            matrixT [[  0.0;  1.0;  0.0;  0.0 ]
                     [  0.0;  0.0; -1.0;  0.0 ]
                     [ -1.0;  0.0;  0.0;  0.0 ]
                     [  0.0;  0.0;  0.0;  1.0 ]]
        ]
    
    let mutable counter = 0
    
    [<CustomEquality; CustomComparison>]
    type T =
        private { Matrix : Matrix<double>; Id : int }
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? T as p -> (this :> IComparable<_>).CompareTo p
                | _ -> -1
        interface IComparable<T> with
            member this.CompareTo other = compare (other.GetPoint()) (this.GetPoint())
        member private this.GetPoint() =
            let a = this.Matrix |> Matrix.toArray2
            (int a[0,0],int a[1,0],int a[2,0])
        override this.Equals other =
            match other with
            | :? T as p -> p.GetPoint() = this.GetPoint()
            | _ -> false
        override this.GetHashCode () = hash (this.GetPoint())
    
    let createWithId (x : int,y : int,z : int) id =
        { Matrix = matrix [[double x];[double y];[double z];[1.0]]; Id = id }
    
    let create (x,y,z) =
        let p = createWithId (x,y,z) counter
        counter <- counter + 1
        p
    
    let get p =
        let a = p.Matrix |> Matrix.toArray2
        (int a[0,0],int a[1,0],int a[2,0])
    
    let getId p = p.Id
    
    let transform (t : Transform) (p : T) = { Matrix = t.Matrix*p.Matrix; Id = p.Id }

let a() =
    let scanners =
        Day19
        |> splitOnBlankLines
        |> Seq.map (
            getStringPerLine
            >> Seq.skip 1
            >> Seq.map (fun s ->
                let p = s |> split "," |> Seq.map int |> Seq.toList
                match p with
                | [x;y;z] -> Point.create (x,y,z)
                | _ -> failwith $"Bad point: %s{s}")
            >> Seq.toList)
        |> Seq.toList
    
    use tw = new StreamWriter(File.OpenWrite("out.txt"))
    
    let overlappingPoints =
        scanners
        |> Seq.mapi (fun i scanner1 ->
            Point.transformations
            |> Seq.collect
                (fun t ->
                    let scanner1 = scanner1 |> List.map (Point.transform t)
                    scanners
                    |> Seq.skip (i + 1)
                    |> Seq.mapi (fun j scanner2 ->
                        let j = j + i + 1
                        scanner1
                        |> Seq.collect (fun pBase1 ->
                            let xBase1,yBase1,zBase1 = pBase1 |> Point.get
                            let set1 =
                                scanner1
                                |> Seq.map (fun p1 ->
                                    let x1,y1,z1 = p1 |> Point.get
                                    Point.createWithId (x1 - xBase1,y1 - yBase1,z1 - zBase1) (p1 |> Point.getId))
                                |> Set.ofSeq
                            scanner2
                            |> Seq.map (fun pBase2 ->
                                let xBase2,yBase2,zBase2 = pBase2 |> Point.get
                                let set2 =
                                    scanner2
                                    |> Seq.map (fun p2 ->
                                        let x2,y2,z2 = p2 |> Point.get
                                        Point.createWithId (x2 - xBase2,y2 - yBase2,z2 - zBase2) (p2 |> Point.getId))
                                    |> Set.ofSeq
                                let overlappingPoints = Set.intersect set1 set2
                                if overlappingPoints |> Set.count >= 12 then
                                    let overlappingPoints =
                                        overlappingPoints
                                        |> Seq.map (fun p ->
                                            let x,y,z = p |> Point.get
                                            (
                                                set1 |> Set.filter (Point.get >> ((=) (x,y,z))) |> Seq.exactlyOne |> Point.getId,
                                                set2 |> Set.filter (Point.get >> ((=) (x,y,z))) |> Seq.exactlyOne |> Point.getId
                                            ),
                                            (x,y,z))
                                        |> Seq.sort
                                        |> Seq.toList
                                    Some ((i,j),(overlappingPoints |> Seq.map fst |> Seq.toList)) else None)
                                    //Some (overlappingPoints,((i,j),(xBase2 - xBase1,yBase2 - yBase1,zBase2 - zBase1),t)) else None)
                            |> Seq.choose id)

                        |> Utils.tryMaxBy (snd >> List.length)
                        |> Option.toList
                        |> List.map (fun o -> printfn "%A" o; snd o))
                    |> Seq.collect id))
        |> Seq.collect id
    
    //let overlappingPoints = overlappingPoints |> Seq.sortBy (fun (_,((i,j),(_,_,_),_)) -> i,j) |> Seq.toList
    //printfn "%A" overlappingPoints
    let overlappingPoints =
        overlappingPoints
        |> Seq.collect id
        |> Utils.groupTuples
        |> Seq.collect (fun (key,values) ->
            let values = key::values |> List.sortDescending
            match values with
            | value::keys -> keys |> Seq.map (fun key -> key,value)
            | _ -> failwith "Invalid list of key and values.")
        |> Utils.groupTuplesAndMap Seq.max
        |> Map.ofSeq
    
    let rec mapId id =
        match overlappingPoints |> Map.tryFind id with
        | None -> id
        | Some id -> mapId id
    
    scanners |> Seq.collect id |> Seq.distinctBy (Point.getId >> mapId) |> Seq.length

let b() =
    ()