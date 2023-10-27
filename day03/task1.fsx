open System
open System.IO

let input = File.ReadAllLines "./input.txt" |> Array.toSeq

type Rucksack = { CompartmentOne: Set<char>; CompartmentTwo: Set<char> }

let getRucksack line =
  let compartments =
    line
    |> Seq.splitInto 2
    |> Seq.map set
    |> Seq.toList
  
  let [one; two] = compartments
  { CompartmentOne = one; CompartmentTwo = two }

let getDupe rucksack =
  Set.intersect rucksack.CompartmentOne rucksack.CompartmentTwo
  |> Seq.head

let getScore item =
  if Char.IsLower item then int item - 96
  else int item - 38

input
|> Seq.map (getRucksack >> getDupe >> getScore)
|> Seq.sum
|> printfn "%d"