open System
open System.IO

let input = File.ReadAllLines "./input.txt" |> Array.toSeq

let rec getGroups (lines:seq<string>) =
  if Seq.isEmpty lines then List.empty
  else
    let group = 
      lines
      |> Seq.take 3
      |> Seq.map set
      |> Seq.toList
    let tail = lines |> Seq.skip 3

    group::(getGroups tail)

let getCommonItem (group:List<Set<char>>) =
  let [one; two; three] = group |> Seq.toList
  
  one
  |> Set.intersect two
  |> Set.intersect three
  |> Seq.head

let getScore item =
  if Char.IsLower item then int item - 96
  else int item - 38

input
|> getGroups
|> Seq.map (getCommonItem >> getScore)
|> Seq.sum
|> printfn "%d"