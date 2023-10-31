open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines "./input.txt" |> Array.toList

type Instruction = {
  From:int
  To:int
  Count:int
}

let parseStackRow (row:seq<char>) =
  row
  |> Seq.chunkBySize 4
  |> Seq.map (function
    | [| '['; crate; ']' |] -> Some crate
    | [| '['; crate; ']'; ' ' |] -> Some crate
    | _ -> None)
  |> List.ofSeq

let parseStacks (lines:List<string>) =
  let rows = 
    lines
    |> List.takeWhile ((<>) "")
    |> List.rev
    |> List.skip 1
    |> List.rev
    |> List.map parseStackRow

  [ for i in 0 .. (rows.[0] |> List.length) - 1 ->
      rows 
      |> List.map (fun row -> row.[i]) 
      |> List.choose id ]

let buildInstruction (regexMatch:Match) =
  { Count = regexMatch.Groups.[1].Value |> int
    From = regexMatch.Groups.[2].Value |> int |> (fun x-> x - 1)
    To = regexMatch.Groups.[3].Value |> int |> (fun x-> x - 1)  }

let parseInstruction (line:string) =
  line
  |> Regex("move (\d+) from (\d) to (\d)").Match
  |> buildInstruction

let parseInstructions (lines:List<string>) =
  lines
  |> List.skipWhile ((<>) "")
  |> List.skip 1
  |> List.map parseInstruction

// let executeInstruction (stacks:List<List<char>>) instruction =
//   let rec loop times stacks =
//     if times = 0 then stacks // exit recursive stack
//     else 
//       let crate = 
//         stacks 
//         |> List.item instruction.From 
//         |> Seq.last

//       stacks 
//       |> List.mapi (fun i e -> 
//         if i = (instruction.To) then List.append stacks[instruction.To] [ crate ]
//         elif i = (instruction.From) then List.take ((List.length stacks[instruction.From])-1) stacks[instruction.From]
//         else e)
//       |> loop (times-1)
  
//   loop instruction.Count stacks

let executeInstruction (stacks:List<List<char>>) (instruction:Instruction) =
  let cratesToMove =
    stacks
    |> List.item instruction.From
    |> List.take instruction.Count
    |> List.rev

  stacks
  |> List.mapi (fun i e ->
    match i with
    | t when t = instruction.To -> List.append cratesToMove stacks[instruction.To]
    | f when f = instruction.From -> stacks[instruction.From] |> List.skip instruction.Count
    | _ -> stacks[i])

let stacks = input |> parseStacks

input
|> parseInstructions
|> List.fold executeInstruction stacks
|> List.map (fun l -> l |> List.head |> printf "%c")
printfn ""
