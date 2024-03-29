module AdventOfCodeCommon.InputUtils

open System

let newLine = Environment.NewLine

let splitWithOptions (separator : string) options (s : string) = s.Split(separator,options)
let split (separator : string) (s : string) = s.Split separator

let splitLinesWithOptions options (s : string) = s |> splitWithOptions newLine options
let splitLines (s : string) = s |> split newLine

let splitOnBlankLines (s : string) = s |> split $"%s{newLine}%s{newLine}"

let getPerLine f (s : string) = s |> splitLines |> Array.map f
let getIntPerLine (s : string) = s |> getPerLine int
let getStringPerLine (s : string) = s |> splitLines

let substring startIndex length (s : string) = s.Substring(startIndex, length)
let substringToEnd startIndex (s : string) = s.Substring startIndex