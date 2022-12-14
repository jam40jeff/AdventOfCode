module AdventOfCode2022.Code.Day7

open System.Collections.Generic
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

type Dir = { Name : string; Items : List<FsItem>; Parent : Dir option }
and File = { Name : string; Size : int64 }
and FsItem = Dir of Dir | File of File

type DirWithSize = { Dirs : DirWithSize list; Size : int64 }

let parseInput() =
    let root = { Name = "/"; Items = List<FsItem>(); Parent = None }
    let rec findRoot dir = match dir.Parent with Some dir -> findRoot dir | None -> dir
    let aggregate dir (cmd : string) =
        let cdCmdPrefix = "$ cd "
        let dirCmdPrefix = "dir "
        if cmd.StartsWith(cdCmdPrefix) then
            let dirName = cmd.Substring cdCmdPrefix.Length
            if dirName = ".." then dir.Parent |> Option.defaultWith (fun () -> failwith "Can't cd .. from root dir.")
            else if dirName = "/" then findRoot dir
            else
                dir.Items
                |> Seq.choose (fun item ->
                    match item with Dir dir -> (if dir.Name = dirName then Some dir else None) | File _ -> None)
                |> Seq.tryExactlyOne
                |> Option.defaultWith (fun () -> failwith $"Could not find directory {dirName} from command {cmd}.")
        else if cmd = "$ ls" then dir
        else if cmd.StartsWith(dirCmdPrefix) then
            let dirName = cmd.Substring dirCmdPrefix.Length
            dir.Items.Add(Dir({ Name = dirName; Items = List<FsItem>(); Parent = Some dir }))
            dir
        else
            let parts = cmd.Split(' ')
            let filename = parts[1]
            let size = int64 parts[0]
            dir.Items.Add(File({ Name = filename; Size = size }))
            dir
    Day7
    |> getStringPerLine
    |> Array.fold aggregate root
    |> findRoot

let rec calculateDirSizes dir =
    let items =
        dir.Items
        |> Seq.map (function
                    | Dir dir ->
                        let dir = calculateDirSizes dir
                        (Some dir, dir.Size)
                    | File file -> (None, file.Size))
        |> Seq.toList
    { Dirs = items |> List.map fst |> List.choose id; Size = items |> List.map snd |> List.sum }

let rec findDirs predicate dir =
    List.append
        (dir.Dirs |> List.collect (findDirs predicate))
        ((if predicate dir then Some dir else None) |> Option.toList)

let getAllDirs = findDirs (fun _ -> true)

let a() =
    let rootDir = parseInput() |> calculateDirSizes
    rootDir |> findDirs (fun dir -> dir.Size < 100000L) |> Seq.map (fun dir -> dir.Size) |> Seq.sum

let b() =
    let rootDir = parseInput() |> calculateDirSizes
    let sizeToDelete = rootDir.Size - 40000000L
    let dirToDelete =
        rootDir
        |> getAllDirs
        |> Seq.filter (fun dir -> dir.Size >= sizeToDelete)
        |> Seq.sortBy (fun dir -> dir.Size)
        |> Seq.head
    dirToDelete.Size