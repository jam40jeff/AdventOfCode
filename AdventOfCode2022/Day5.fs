module AdventOfCode2022.Code.Day5

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Checked
open AdventOfCodeCommon.InputUtils
open type AdventOfCodeInput.Input2022

let regex = Regex(@"move ([0-9]+) from ([0-9]+) to ([0-9]+)")

type Instruction = { Quantity : int; From : int; To : int }

let private parseInput() =
    let parseStacks (s : string) =
        let parseStackLine (s : string) =
            let numStacks = s.Length / 4 + 1
            let parseStackChar n =
                let c = s[n * 4 + 1]
                if c = ' ' then None else Some c
            if s |> Seq.exists Char.IsDigit then None else Seq.init numStacks parseStackChar |> Seq.toList |> Some
        let stackLines = s |> getPerLine parseStackLine |> Seq.choose id
        let numStacks = stackLines |> Seq.map (fun l -> l.Length) |> Seq.max
        let stacks = Seq.init numStacks (fun _ -> Stack<char>()) |> Seq.toList
        let pushOntoStack i c = c |> Option.iter stacks[i].Push
        stackLines |> Seq.rev |> Seq.iter (fun stackLine -> stackLine |> Seq.iteri pushOntoStack)
        stacks
        
    let parseInstructions (s : string) =
        let parseInstruction (s : string) =
            let m = regex.Match s
            { Quantity = int m.Groups[1].Value; From = int m.Groups[2].Value; To = int m.Groups[3].Value }
        s |> getPerLine parseInstruction
    
    let parts = Day5 |> splitOnBlankLines
    parts[0] |> parseStacks,parts[1] |> parseInstructions

let a() =
    let stacks,instructions = parseInput()
    let applyInstruction instruction =
        Seq.init instruction.Quantity id
        |> Seq.iter (fun _ -> stacks[instruction.To - 1].Push(stacks[instruction.From - 1].Pop()))
    instructions |> Seq.iter applyInstruction
    String(stacks |> Seq.map (fun stack -> stack.Pop()) |> Seq.toArray)
    
let b() =
    let stacks,instructions = parseInput()
    let applyInstruction instruction =
        Seq.init instruction.Quantity id
        |> Seq.map (fun _ -> stacks[instruction.From - 1].Pop())
        |> Seq.rev
        |> Seq.iter stacks[instruction.To - 1].Push
    instructions |> Seq.iter applyInstruction
    String(stacks |> Seq.map (fun stack -> stack.Pop()) |> Seq.toArray)