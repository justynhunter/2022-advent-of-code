open System
open System.IO
open System.Text.RegularExpressions

type Instruction = {
  From:int
  To:int
  Count:int
}

let parseStacks (lines:List<string>) =
  let rows = 
    lines
    |> List.takeWhile ((<>) "")
    |> List.rev
    |> List.skip 1
    |> List.rev
    |> List.map (fun row -> 
      row
      |> Seq.chunkBySize 4
      |> Seq.map (function
      | [| '['; crate; ']' |] -> Some crate
      | [| '['; crate; ']'; ' ' |] -> Some crate
      | _ -> None)
      |> List.ofSeq)

  [ for i in 0 .. (rows.[0] |> List.length) - 1 ->
      rows 
      |> List.map (fun row -> row.[i]) 
      |> List.choose id ]

let parseInstructions (lines:List<string>) =
  lines
  |> List.skipWhile ((<>) "")
  |> List.skip 1
  |> List.map (fun line -> 
    line
    |> Regex("move (\d+) from (\d) to (\d)").Match
    |> (fun rm -> 
      { Count = rm.Groups.[1].Value |> int
        From = (int rm.Groups.[2].Value) - 1
        To = (int rm.Groups.[3].Value) - 1 }))

let executeInstruction (stacks:List<List<char>>) (instruction:Instruction) =
  let cratesToMove =
    stacks
    |> List.item instruction.From
    |> List.take instruction.Count
    |> List.rev

  stacks
  |> List.mapi (fun i e ->
    match i with
    | t when t = instruction.To -> 
      stacks[instruction.To]
      |> List.append cratesToMove
    | f when f = instruction.From -> 
      stacks[instruction.From] 
      |> List.skip instruction.Count
    | _ -> stacks[i])

let run =
  let input = File.ReadAllLines "./input.txt" |> Array.toList
  let stacks = input |> parseStacks

  input
  |> parseInstructions
  |> List.fold executeInstruction stacks
  

run
|> List.map (fun l ->
  l 
  |> List.head 
  |> printf "%c")
|> ignore
printfn ""
1
