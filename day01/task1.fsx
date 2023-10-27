open System
open System.IO

let input = File.ReadAllLines "./input.txt" |> List.ofSeq 

let hasString line =
  line <> ""

let rec getByElf lines =
  if lines |> List.isEmpty then
    []
  else
    let linesForElf = 
      lines
      |> List.takeWhile hasString

    let tail =
      lines
      |> List.skipWhile hasString
      |> List.skip 1

    linesForElf::(getByElf tail)

let sum lines =
  List.append lines [""]
    |> getByElf 
    |> List.map (fun l -> List.map int l) 
    |> List.map List.sum 
    |> List.max

sum input
    |> printfn "%d"

